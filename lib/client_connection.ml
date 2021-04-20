(*----------------------------------------------------------------------------
 *  Copyright (c) 2017 Inhabited Type LLC.
 *  Copyright (c) 2019 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module AB = Angstrom.Buffered
module Reader = Parse.Reader
module Writer = Serialize.Writer

module Scheduler = Scheduler.Make (struct
  include Stream

  type t = Respd.t

  let flush_write_body = Respd.flush_request_body

  let requires_output = Respd.requires_output
end)

module Queue = struct
  include Queue

  let take_opt t = match is_empty t with true -> None | false -> Some (take t)
end

type error = Respd.error

type trailers_handler = Headers.t -> unit

type response_handler = Response.t -> [ `read ] Body.t -> unit

type error_handler = error -> unit

type t =
  { mutable settings : Settings.t
  ; reader : Reader.frame
  ; writer : Writer.t
  ; config : Config.t
  ; streams : Scheduler.t
  ; mutable current_stream_id : Stream_identifier.t
  ; mutable max_pushed_stream_id : Stream_identifier.t
  ; mutable current_server_streams : int
  ; mutable receiving_headers_for_stream : Stream_identifier.t option
  ; mutable did_send_go_away : bool
  ; mutable unacked_settings : int
  ; pending_pings : (unit -> unit) Queue.t
  ; error_handler : error -> unit
  ; push_handler : Request.t -> (response_handler, unit) result
        (* From RFC7540§4.3:
         *   Header compression is stateful. One compression context and one
         *   decompression context are used for the entire connection. *)
  ; hpack_encoder : Hpack.Encoder.t
  ; hpack_decoder : Hpack.Decoder.t
  }

let default_push_handler = Sys.opaque_identity (fun _ -> Ok (fun _ _ -> ()))

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer

let shutdown_reader t = Reader.force_close t.reader

let flush_request_body t =
  Scheduler.flush t.streams (t.current_stream_id, t.max_pushed_stream_id)

let shutdown_writer t =
  flush_request_body t;
  Writer.close t.writer

let shutdown_rw t =
  shutdown_reader t;
  shutdown_writer t

(* Handling frames against closed streams is hard. See:
 * https://docs.google.com/presentation/d/1iG_U2bKTc9CnKr0jPTrNfmxyLufx_cK2nNh9VjrKH6s
 *)
let was_closed_or_implicitly_closed t stream_id =
  if Stream_identifier.is_request stream_id then
    Stream_identifier.(stream_id <= t.current_stream_id)
  else
    Stream_identifier.(stream_id <= t.max_pushed_stream_id)

let report_error t = function
  | Error.ConnectionError (error, data) ->
    if not t.did_send_go_away then (
      (* From RFC7540§5.4.1:
       *   An endpoint that encounters a connection error SHOULD first send a
       *   GOAWAY frame (Section 6.8) with the stream identifier of the last
       *   stream that it successfully received from its peer. The GOAWAY frame
       *   includes an error code that indicates why the connection is
       *   terminating. After sending the GOAWAY frame for an error condition,
       *   the endpoint MUST close the TCP connection. *)
      let debug_data =
        if String.length data = 0 then
          Bigstringaf.empty
        else
          Bigstringaf.of_string ~off:0 ~len:(String.length data) data
      in
      let frame_info = Writer.make_frame_info Stream_identifier.connection in
      (* TODO: Only write if not already shutdown. *)
      Writer.write_go_away
        t.writer
        frame_info
        ~debug_data
        ~last_stream_id:
          (if Stream_identifier.(t.current_stream_id === -1l) then
             Stream_identifier.connection
          else
            t.current_stream_id)
        error;
      if error <> Error_code.NoError then
        t.error_handler (`Protocol_error (error, data));
      Writer.flush t.writer (fun () ->
          (* XXX: We need to allow lower numbered streams to complete before
           * shutting down. *)
          shutdown_rw t);
      t.did_send_go_away <- true;
      Writer.wakeup t.writer)
  | StreamError (stream_id, error) ->
    (match Scheduler.find t.streams stream_id with
    | Some respd ->
      Respd.report_error respd (`Protocol_error (error, "")) error
    | None ->
      if not (was_closed_or_implicitly_closed t stream_id) then
        (* Possible if the stream was going to enter the Idle state (first time
         * we saw e.g. a PRIORITY frame for it) but had e.g. a
         * FRAME_SIZE_ERROR. *)
        let frame_info = Writer.make_frame_info stream_id in
        Writer.write_rst_stream t.writer frame_info error);
    Writer.wakeup t.writer

let report_connection_error t ?(additional_debug_data = "") error =
  report_error t (ConnectionError (error, additional_debug_data))

let report_stream_error t stream_id error =
  report_error t (StreamError (stream_id, error))

let shutdown t =
  (* From RFC7540§6.8:
   *   A server that is attempting to gracefully shut down a connection SHOULD
   *   send an initial GOAWAY frame with the last stream identifier set to
   *   2^31-1 and a NO_ERROR code. *)
  report_connection_error t Error_code.NoError

let set_error_and_handle t stream error error_code =
  Respd.report_error stream error error_code;
  Writer.wakeup t.writer

let report_exn t exn =
  if not (is_closed t) then
    let additional_debug_data = Printexc.to_string exn in
    report_connection_error t ~additional_debug_data Error_code.InternalError

let send_window_update
    : type a. t -> a Scheduler.PriorityTreeNode.node -> int32 -> unit
  =
 fun t stream n ->
  let send_window_update_frame stream_id n =
    let valid_inflow = Scheduler.add_inflow stream n in
    assert valid_inflow;
    let frame_info = Writer.make_frame_info stream_id in
    Writer.write_window_update t.writer frame_info n
  in
  if Int32.compare n 0l > 0 then (
    let max_window_size = Settings.WindowSize.max_window_size in
    let stream_id = Scheduler.stream_id stream in
    let rec loop n =
      if n > max_window_size then (
        send_window_update_frame stream_id max_window_size;
        loop (Int32.sub n max_window_size))
      else
        send_window_update_frame stream_id n
    in
    loop n;
    Writer.wakeup t.writer)

let handle_push_promise_headers t respd headers =
  (* From RFC7540§8.2.2:
   *   The header fields in PUSH_PROMISE and any subsequent CONTINUATION frames
   *   MUST be a valid and complete set of request header fields (Section
   *   8.1.2.3). *)
  match Headers.method_path_and_scheme_or_malformed headers with
  | `Malformed ->
    (* From RFC7540§8.2.2:
     *   If a client receives a PUSH_PROMISE that does not include a complete
     *   and valid set of header fields [...] it MUST respond with a stream
     *   error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t respd.Stream.id Error_code.ProtocolError
  | `Valid (meth, path, scheme) ->
    let meth = Httpaf.Method.of_string meth in
    (match
       meth, Headers.get_pseudo headers "authority", Message.body_length headers
     with
    | (#Httpaf.Method.standard as meth), _, _
      when not Httpaf.Method.(is_cacheable meth && is_safe meth) ->
      report_stream_error t respd.id Error_code.ProtocolError
    | _, _, `Fixed len when not (Int64.equal len 0L) ->
      (* From RFC7540§8.2:
       *   Clients that receive a promised request that is not cacheable,
       *   that is not known to be safe or that indicates the presence of a
       *   request body MUST reset the promised stream with a stream error
       *   (Section 5.4.2) of type PROTOCOL_ERROR.
       *
       * From RFC7231§4.2.3 (Cacheable Methods):
       *   [...] this specification defines GET, HEAD, and POST as cacheable
       *   [...].
       *
       * From RFC7231§4.2.1 (Safe Methods):
       *   Of the request methods defined by this specification, the GET, HEAD,
       *   OPTIONS, and TRACE methods are defined to be safe.
       *
       * Note: the intersection of safe and cacheable are the GET and HEAD
       * methods. *)
      report_stream_error t respd.id Error_code.ProtocolError
    (* From RFC7540§8.2:
     *   The server MUST include a value in the :authority pseudo-header field
     *   for which the server is authoritative (see Section 10.1). A client
     *   MUST treat a PUSH_PROMISE for which the server is not authoritative as
     *   a stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    | _, None, _ | _, _, `Error _ ->
      report_stream_error t respd.id Error_code.ProtocolError
    | _ ->
      let request = Request.create ~scheme ~headers meth path in
      (match t.push_handler request with
      | Ok response_handler ->
        (* From RFC7540§8.2:
         *   Promised requests [...] MUST NOT include a request body. *)
        let request_body = Body.empty in
        (* From RFC7540§5.1:
         *   reserved (remote): [...] Receiving a HEADERS frame causes the
         *   stream to transition to "half-closed (local)". *)
        respd.state <-
          Active
            ( HalfClosed Stream.WaitingForPeer
            , { Respd.request
              ; request_body
              ; response_handler
              ; trailers_handler = ignore
              } )
      | Error _ ->
        (* From RFC7540§6.6:
         *   Recipients of PUSH_PROMISE frames can choose to reject promised
         *   streams by returning a RST_STREAM referencing the promised stream
         *   identifier back to the sender of the PUSH_PROMISE. *)
        Stream.reset_stream respd Error_code.Cancel))

let handle_response_headers t stream ~end_stream active_request headers =
  let (Scheduler.Stream { descriptor = respd; _ }) = stream in
  (* From RFC7540§8.1.2.6:
   *   Clients MUST NOT accept a malformed response.
   *
   * Note: in the branches where a malformed response is detected, the response
   * handler is not called. *)
  match Headers.get_multi_pseudo headers "status" with
  | [ status ] ->
    let response = Response.create ~headers (Status.of_string status) in
    (* Note: we don't need to check for `end_stream` flag + a non-zero body
     * length, as the spec allows for non-zero content-length headers and no
     * DATA frames.
     *
     * From RFC7540§8.1.2.6:
     *   A response that is defined to have no payload, as described in
     *   [RFC7230], Section 3.3.2, can have a non-zero content-length header
     *   field, even though no content is included in DATA frames. *)
    (match Message.body_length headers with
    | `Error _ ->
      set_error_and_handle
        t
        respd
        (`Invalid_response_body_length response)
        ProtocolError
    | `Fixed _ | `Unknown ->
      let response_body =
        if end_stream then
          Body.empty
        else
          Body.create_reader
            (Bigstringaf.create t.config.response_body_buffer_size)
            ~done_reading:(fun len ->
              let len = Int32.of_int len in
              send_window_update t t.streams len;
              send_window_update t stream len)
      in
      let new_response_state =
        Respd.create_active_response response response_body
      in
      respd.state <-
        Active
          ( (if Stream.is_open respd then
               Open new_response_state
            else
              HalfClosed new_response_state)
          , active_request );
      active_request.response_handler response response_body;
      if end_stream then (
        (* Deliver EOF to the response body, as the handler might be waiting
         * on it to act. *)
        Body.close_reader response_body;
        (* From RFC7540§5.1:
         *   [...] an endpoint receiving an END_STREAM flag causes the stream
         *   state to become "half-closed (remote)". *)
        Respd.close_stream respd))
  | _ ->
    (* From RFC7540§8.1.2.4:
     *   For HTTP/2 responses, a single :status pseudo-header field is defined
     *   that carries the HTTP status code field (see [RFC7231], Section 6).
     *   This pseudo-header field MUST be included in all responses; otherwise,
     *   the response is malformed (Section 8.1.2.6). *)
    let message =
      "HTTP/2 responses must include a single `:status` pseudo-header"
    in
    set_error_and_handle t respd (`Malformed_response message) ProtocolError

let handle_headers t ~end_stream stream headers =
  let (Scheduler.Stream { descriptor = respd; _ }) = stream in
  (* From RFC7540§5.1.2:
   *   Endpoints MUST NOT exceed the limit set by their peer. An endpoint that
   *   receives a HEADERS frame that causes its advertised concurrent stream
   *   limit to be exceeded MUST treat this as a stream error (Section 5.4.2)
   *   of type PROTOCOL_ERROR or REFUSED_STREAM. *)
  if
    Int32.(
      compare
        (of_int (t.current_server_streams + 1))
        t.config.max_concurrent_streams)
    > 0
  then
    if t.unacked_settings > 0 then
      (* From RFC7540§8.1.4:
       *   The REFUSED_STREAM error code can be included in a RST_STREAM frame
       *   to indicate that the stream is being closed prior to any processing
       *   having occurred. Any request that was sent on the reset stream can
       *   be safely retried.
       *
       * Note: if there are pending SETTINGS to acknowledge, assume there was a
       * race condition and let the client retry. *)
      report_stream_error t respd.Stream.id Error_code.RefusedStream
    else
      report_stream_error t respd.Stream.id Error_code.ProtocolError
  else (
    (* From RFC7540§5.1.2:
     *   Streams that are in the "open" state or in either of the "half-closed"
     *   states count toward the maximum number of streams that an endpoint is
     *   permitted to open. *)
    t.current_server_streams <- t.current_server_streams + 1;
    match respd.state with
    | Reserved _ ->
      respd.state <- Reserved Stream.FullHeaders;
      handle_push_promise_headers t respd headers
    | Active (active_state, active_request) ->
      (match active_state with
      | Open _ ->
        respd.state <- Active (Open FullHeaders, active_request)
      | HalfClosed _ ->
        respd.state <- Active (HalfClosed FullHeaders, active_request));
      handle_response_headers t stream ~end_stream active_request headers
    | _ ->
      (* Unreachable. This function is only invoked if the stream is active. *)
      assert false)

let handle_headers_block
    t ?(is_trailers = false) stream partial_headers flags headers_block
  =
  let open AB in
  let (Scheduler.Stream { descriptor = respd; _ }) = stream in
  let end_headers = Flags.test_end_header flags in
  (* From RFC7540§6.10:
   *   An endpoint receiving HEADERS, PUSH_PROMISE, or CONTINUATION
   *   frames needs to reassemble header blocks and perform decompression
   *   even if the frames are to be discarded *)
  let parse_state' =
    AB.feed partial_headers.Stream.parse_state (`Bigstring headers_block)
  in
  if end_headers then (
    t.receiving_headers_for_stream <- None;
    let parse_state' = AB.feed parse_state' `Eof in
    match parse_state' with
    | Done (_, Ok headers) ->
      if not is_trailers then
        (* `handle_headers` will take care of transitioning the stream state *)
        let end_stream = partial_headers.end_stream in
        handle_headers t ~end_stream stream headers
      else if Headers.trailers_valid headers then (
        Respd.deliver_trailer_headers respd headers;
        let response_body = Respd.response_body_exn respd in
        Body.close_reader response_body)
      else
        (* From RFC7540§8.1.2.1:
         *   Pseudo-header fields MUST NOT appear in trailers. Endpoints MUST
         *   treat a request or response that contains undefined or invalid
         *   pseudo-header fields as malformed (Section 8.1.2.6). *)
        let message = "Pseudo-header fields must not appear in trailers" in
        set_error_and_handle t respd (`Malformed_response message) ProtocolError
    (* From RFC7540§4.3:
     *   A decoding error in a header block MUST be treated as a connection
     *   error (Section 5.4.1) of type COMPRESSION_ERROR. *)
    | Done (_, Error _) | Partial _ ->
      report_connection_error t Error_code.CompressionError
    | Fail (_, _, message) ->
      report_connection_error
        t
        ~additional_debug_data:message
        Error_code.CompressionError)
  else
    partial_headers.parse_state <- parse_state'

let handle_trailer_headers = handle_headers_block ~is_trailers:true

let create_partial_headers t flags headers_block =
  let end_headers = Flags.test_end_header flags in
  let headers_block_length = Bigstringaf.length headers_block in
  let initial_buffer_size =
    if end_headers then
      headers_block_length
    else
      (* Conservative estimate that there's only going to be one CONTINUATION
       * frame. *)
      2 * headers_block_length
  in
  { Stream.parse_state =
      AB.parse
        ~initial_buffer_size
        (Hpack.Decoder.decode_headers t.hpack_decoder)
  ; end_stream = Flags.test_end_stream flags
  }

let handle_first_response_bytes
    t stream active_request frame_header headers_block
  =
  let (Scheduler.Stream { descriptor; _ }) = stream in
  let { Frame.flags; stream_id; _ } = frame_header in
  let partial_headers = create_partial_headers t flags headers_block in
  let remote_state = Stream.PartialHeaders partial_headers in
  descriptor.Stream.state <-
    (if Stream.is_open descriptor then
       Active (Open remote_state, active_request)
    else
      Active (HalfClosed remote_state, active_request));
  if not (Flags.test_end_header flags) then
    t.receiving_headers_for_stream <- Some stream_id;
  handle_headers_block t stream partial_headers flags headers_block

let process_trailer_headers t stream active_response frame_header headers_block =
  let (Scheduler.Stream { descriptor = respd; _ }) = stream in
  let { Frame.stream_id; flags; _ } = frame_header in
  let end_stream = Flags.test_end_stream flags in
  if not end_stream then
    (* From RFC7540§8.1:
     *   A HEADERS frame (and associated CONTINUATION frames) can only appear
     *   at the start or end of a stream. An endpoint that receives a HEADERS
     *   frame without the END_STREAM flag set after receiving a final
     *   (non-informational) status code MUST treat the corresponding request
     *   or response as malformed (Section 8.1.2.6). *)
    let message =
      "HEADERS frames containing trailers must set the END_STREAM flag"
    in
    set_error_and_handle t respd (`Malformed_response message) ProtocolError
  else
    let partial_headers =
      { Stream.parse_state =
          AB.parse (Hpack.Decoder.decode_headers t.hpack_decoder)
          (* obviously true at this point. *)
      ; end_stream
      }
    in
    active_response.Respd.trailers_parser <- Some partial_headers;
    if not Flags.(test_end_header flags) then
      t.receiving_headers_for_stream <- Some stream_id;
    (* trailer headers: RFC7230§4.4 *)
    handle_trailer_headers t stream partial_headers flags headers_block

let process_headers_frame t { Frame.frame_header; _ } headers_block =
  let { Frame.stream_id; _ } = frame_header in
  match Scheduler.get_node t.streams stream_id with
  | None ->
    (* If we're receiving a response for a stream that's no longer in the
     * priority tree, assume this is a network race - we canceled a request
     * but a responnse was already in flight.
     *
     * However, if the stream identifer is greater than the largest stream
     * identifier we have produced, they should know better. In this case,
     * send an RST_STREAM. *)
    if
      Stream_identifier.(
        stream_id >= t.current_stream_id && is_request stream_id)
    then
      report_stream_error t stream_id Error_code.StreamClosed
  | Some (Scheduler.Stream { descriptor; _ } as stream) ->
    (match descriptor.state with
    | Idle ->
      (* From RFC7540§6.2:
       *   HEADERS frames can be sent on a stream in the "idle", "reserved
       *   (local)", "open", or "half-closed (remote)" state. *)
      report_connection_error t Error_code.ProtocolError
    | Active ((Open WaitingForPeer | HalfClosed WaitingForPeer), active_request)
      ->
      handle_first_response_bytes
        t
        stream
        active_request
        frame_header
        headers_block
    | Active
        ( ( Open (FullHeaders | PartialHeaders _)
          | HalfClosed (FullHeaders | PartialHeaders _) )
        , _ ) ->
      assert false
    (* if we're getting a HEADERS frame at this point, they must be
     * trailers, and the END_STREAM flag needs to be set. *)
    | Active
        ( ( Open (ActiveMessage active_response)
          | HalfClosed (ActiveMessage active_response) )
        , _ ) ->
      process_trailer_headers
        t
        stream
        active_response
        frame_header
        headers_block
    | Closed { reason = ResetByThem _; _ } ->
      (* From RFC7540§5.1:
       *   closed: [...] An endpoint that receives any frame other than
       *   PRIORITY after receiving a RST_STREAM MUST treat that as a
       *   stream error (Section 5.4.2) of type STREAM_CLOSED. *)
      report_stream_error t stream_id Error_code.StreamClosed
    (* From RFC7540§5.1:
     *   reserved (local): [...] Receiving any type of frame other than
     *   RST_STREAM, PRIORITY, or WINDOW_UPDATE on a stream in this state
     *   MUST be treated as a connection error (Section 5.4.1) of type
     *   PROTOCOL_ERROR. *)
    | Reserved _ | Closed _ ->
      (* From RFC7540§5.1:
       *   Similarly, an endpoint that receives any frames after receiving
       *   a frame with the END_STREAM flag set MUST treat that as a
       *   connection error (Section 5.4.1) of type STREAM_CLOSED [...]. *)
      report_connection_error t Error_code.StreamClosed)

let process_data_frame t { Frame.frame_header; _ } bstr =
  let open Scheduler in
  let { Frame.flags; stream_id; payload_length; _ } = frame_header in
  let payload_len32 = Int32.of_int payload_length in
  match Scheduler.get_node t.streams stream_id with
  | Some (Stream { descriptor; _ } as stream) ->
    (match descriptor.state with
    | Active
        ( ( Open (ActiveMessage response_info)
          | HalfClosed (ActiveMessage response_info) )
        , _ ) ->
      let { Respd.response; response_body; response_body_bytes; _ } =
        response_info
      in
      response_info.response_body_bytes <-
        Int64.(add response_body_bytes (of_int (Bigstringaf.length bstr)));
      (* First, calculate whether we're allowed to receive this frame based
       * on the _current_ inflow. *)
      let allowed_to_receive =
        Scheduler.(allowed_to_receive t.streams stream payload_len32)
      in
      (* Then, deduct inflow from the connection flow-control window, as
       * mandated by the protocol.
       *
       * From RFC7540§6.9:
       *   A receiver that receives a flow-controlled frame MUST always account
       *   for its contribution against the connection flow-control window,
       *   unless the receiver treats this as a connection error (Section 5.4.1).
       *   This is necessary even if the frame is in error. *)
      Scheduler.deduct_inflow t.streams payload_len32;
      if not allowed_to_receive then (
        (* From RFC7540§6.9:
         *  A receiver MAY respond with a stream error (Section 5.4.2) or
         *  connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR if it
         *  is unable to accept a frame. *)
        send_window_update t t.streams payload_len32;
        report_stream_error t stream_id Error_code.FlowControlError)
      else (
        Scheduler.deduct_inflow stream payload_len32;
        match Message.body_length response.headers with
        | `Fixed len
        (* Getting more than the server declared *)
          when Int64.compare response_info.response_body_bytes len > 0 ->
          (* Give back connection-level flow-controlled bytes (we use payload
           * length to include any padding bytes that the frame might have
           * included - which were ignored at parse time). *)
          send_window_update t t.streams payload_len32;
          (* From RFC7540§8.1.2.6:
           *   A request or response is also malformed if the value of a
           *   content-length header field does not equal the sum of the
           *   DATA frame payload lengths that form the body. *)
          set_error_and_handle
            t
            descriptor
            (`Invalid_response_body_length response)
            ProtocolError
        | _ ->
          let end_stream = Flags.test_end_stream flags in
          (* From RFC7540§6.9.1:
           *   The receiver of a frame sends a WINDOW_UPDATE frame as it
           *   consumes data and frees up space in flow-control windows.
           *   Separate WINDOW_UPDATE frames are sent for the stream- and
           *   connection-level flow-control windows.
           *
           * Note: we send these WINDOW_UPDATE frames once the body bytes
           * have been surfaced to the application. This is done in the
           * record field `done_reading` of `Body.t`. *)
          let faraday = Body.unsafe_faraday response_body in
          if not (Faraday.is_closed faraday) then (
            Faraday.schedule_bigstring faraday bstr;
            if end_stream then Body.close_reader response_body);
          Respd.flush_response_body descriptor;
          if end_stream && not (Respd.requires_output descriptor) then
            (* From RFC7540§6.1:
             *   When set, bit 0 indicates that this frame is the last that
             *   the endpoint will send for the identified stream. Setting
             *   this flag causes the stream to enter one of the
             *   "half-closed" states or the "closed" state (Section 5.1).
             *
             * Transition to the "closed" state if this is the last DATA frame
             * that the server will send and we're done sending. *)
            Stream.finish_stream descriptor Finished)
    | Idle ->
      (* From RFC7540§5.1:
       *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
       *   a stream in this state MUST be treated as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error_code.ProtocolError
    (* This is technically in the half-closed (local) state *)
    | Closed { reason = ResetByUs NoError; _ } ->
      (* From RFC7540§6.9:
       *   A receiver that receives a flow-controlled frame MUST always
       *   account for its contribution against the connection flow-control
       *   window, unless the receiver treats this as a connection error
       *   (Section 5.4.1). This is necessary even if the frame is in
       *   error. *)
      send_window_update t t.streams payload_len32
    (* From RFC7540§6.4:
     *   [...] after sending the RST_STREAM, the sending endpoint MUST be
     *   prepared to receive and process additional frames sent on the
     *   stream that might have been sent by the peer prior to the arrival
     *   of the RST_STREAM.
     *
     * Note: after some writer yields / wake ups, we will have stopped
     * keeping state information for the stream. This functions effectively
     * as a way of only accepting frames after an RST_STREAM from us up to
     * a time limit. *)
    | _ ->
      send_window_update t t.streams payload_len32;
      (* From RFC7540§6.1:
       *   If a DATA frame is received whose stream is not in "open" or
       *   "half-closed (local)" state, the recipient MUST respond with a
       *   stream error (Section 5.4.2) of type STREAM_CLOSED. *)
      report_stream_error t stream_id Error_code.StreamClosed)
  | None ->
    if not (was_closed_or_implicitly_closed t stream_id) then
      (* From RFC7540§5.1:
       *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
       *   a stream in this state MUST be treated as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error_code.ProtocolError

let on_close_stream t id ~active closed =
  if active then
    (* From RFC7540§5.1.2:
     *   Streams that are in the "open" state or in either of the "half-closed"
     *   states count toward the maximum number of streams that an endpoint is
     *   permitted to open. *)
    t.current_server_streams <- t.current_server_streams - 1;
  Scheduler.mark_for_removal t.streams id closed

let process_priority_frame t { Frame.frame_header; _ } priority =
  let { Frame.stream_id; _ } = frame_header in
  let { Priority.stream_dependency; _ } = priority in
  if Stream_identifier.(stream_id === stream_dependency) then
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a
     *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error_code.ProtocolError
  else
    match Scheduler.get_node t.streams stream_id with
    | Some stream ->
      Scheduler.reprioritize_stream t.streams ~priority stream
    | None ->
      (* From RFC7540§5.3:
       *   A client can assign a priority for a new stream by including
       *   prioritization information in the HEADERS frame (Section 6.2) that
       *   opens the stream. At any other time, the PRIORITY frame (Section
       *   6.3) can be used to change the priority of a stream.
       *
       * Note: The spec mostly only mentions that clients are the endpoints
       *       that make use of PRIORITY frames. As such, we don't make too
       *       much of an effort to process PRIORITY frames coming from a
       *       server. If we know about a stream, we reprioritize it (meaning
       *       prioritization is an input to the process of allocating
       *       resources when flushing request bodies). Otherwise, we ignore
       *       it. We don't, however, report any errors if the frame is
       *       well-formed, as section 5. clearly mentions that PRIORITY frames
       *       must be accepted in all stream states.
       *
       * From RFC7540§5.1:
       *   Note that PRIORITY can be sent and received in any stream state. *)
      ()

let process_rst_stream_frame t { Frame.frame_header; _ } error_code =
  let { Frame.stream_id; _ } = frame_header in
  match Scheduler.find t.streams stream_id with
  | Some respd ->
    (match respd.state, error_code with
    | Idle, _ ->
      (* From RFC7540§6.4:
       *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
       *   state. If a RST_STREAM frame identifying an idle stream is
       *   received, the recipient MUST treat this as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error_code.ProtocolError
    | Closed _, Error_code.NoError ->
      (* From RFC7540§8.1:
       *   A server can send a complete response prior to the client sending an
       *   entire request if the response does not depend on any portion of the
       *   request that has not been sent and received. When this is true, a
       *   server MAY request that the client abort transmission of a request
       *   without error by sending a RST_STREAM with an error code of NO_ERROR
       *   after sending a complete response (i.e., a frame with the END_STREAM
       *   flag).
       *
       * If we're done sending the request there's nothing to do here, allow
       * the stream to finish successfully.
       *)
      (* XXX(anmonteiro): When we add logging support, add something here. *)
      ()
    | Active _, Error_code.NoError ->
      (* If we're active (i.e. not done sending the request body), finish the
       * stream, in order to mark it for cleanup.
       *
       * Note: we don't close the request body here because the client may be
       * in the process of writing to it, and while we're not going to send
       * those bytes to the output channel, we don't want to fail when writing
       * either. *)
      Stream.finish_stream respd (ResetByThem error_code)
    | _ ->
      (* From RFC7540§6.4:
       *   The RST_STREAM frame fully terminates the referenced stream and
       *   causes it to enter the "closed" state. After receiving a
       *   RST_STREAM on a stream, the receiver MUST NOT send additional
       *   frames for that stream, with the exception of PRIORITY.
       *
       * Note:
       *   This match branch also accepts streams in the `Closed` state. We
       *   do that to comply with the following:
       *
       * From RFC7540§6.4:
       *   [...] after sending the RST_STREAM, the sending endpoint MUST be
       *   prepared to receive and process additional frames sent on the
       *   stream that might have been sent by the peer prior to the arrival
       *   of the RST_STREAM. *)
      Stream.finish_stream respd (ResetByThem error_code);
      (* From RFC7540§5.4.2:
       *   To avoid looping, an endpoint MUST NOT send a RST_STREAM in response
       *   to a RST_STREAM frame.
       *
       *   Note: the {!Respd.report_error} function does not send an RST_STREAM
       *   frame for streams in the closed state. So we close the stream before
       *   reporting the error. *)
      set_error_and_handle t respd (`Protocol_error (error_code, "")) error_code)
  | None ->
    (* We might have removed the stream from the hash table. If its stream
     * id is smaller than or equal to the max client stream id we've generated,
     * then it must have been closed. *)
    if not (was_closed_or_implicitly_closed t stream_id) then
      (* From RFC7540§6.4:
       *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
       *   state. If a RST_STREAM frame identifying an idle stream is
       *   received, the recipient MUST treat this as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR.
       *
       * Note:
       *   If we didn't find the stream in the hash table it must be "idle". *)
      report_connection_error t Error_code.ProtocolError

let process_settings_frame t { Frame.frame_header; _ } settings =
  let open Scheduler in
  let { Frame.flags; _ } = frame_header in
  (* We already checked that an acked SETTINGS is empty. Don't need to do
   * anything else in that case *)
  if Flags.(test_ack flags) then (
    t.unacked_settings <- t.unacked_settings - 1;
    if t.unacked_settings < 0 then
      (* The server is ACKing a SETTINGS frame that we didn't send *)
      let additional_debug_data =
        "Received SETTINGS with ACK but no ACK was pending"
      in
      report_connection_error t ~additional_debug_data Error_code.ProtocolError)
  else
    match Settings.check_settings_list ~is_client:true settings with
    | Ok () ->
      (* From RFC7540§6.5:
       *   Each parameter in a SETTINGS frame replaces any existing value for
       *   that parameter. Parameters are processed in the order in which they
       *   appear, and a receiver of a SETTINGS frame does not need to maintain
       *   any state other than the current value of its parameters. *)
      let new_settings =
        List.fold_left
          (fun (acc : Settings.t) item ->
            match item with
            | Settings.HeaderTableSize x ->
              (* From RFC7540§6.5.2:
               *   Allows the sender to inform the remote endpoint of the maximum
               *   size of the header compression table used to decode header
               *   blocks, in octets. *)
              Hpack.Encoder.set_capacity t.hpack_encoder x;
              { acc with header_table_size = x }
            | EnablePush x ->
              (* We've already verified that this setting is either 0 or 1 in the
               * call to `Settings.check_settings_list` above. *)
              { acc with enable_push = x = 1 }
            | MaxConcurrentStreams x ->
              { acc with max_concurrent_streams = x }
            | InitialWindowSize new_val ->
              (* From RFC7540§6.9.2:
               *   In addition to changing the flow-control window for streams
               *   that are not yet active,  a SETTINGS frame can alter the
               *   initial flow-control window size for streams with active
               *   flow-control windows (that is, streams in the "open" or
               *   "half-closed (remote)" state). When the value of
               *   SETTINGS_INITIAL_WINDOW_SIZE changes, a receiver MUST adjust
               *   the size of all stream flow-control windows that it
               *   maintains by the difference between the new value and the
               *   old value.
               *
               *   [...] A SETTINGS frame cannot alter the connection
               *   flow-control window. *)
              let old_val = t.settings.initial_window_size in
              let growth = Int32.sub new_val old_val in
              let exception Local in
              (match
                 Scheduler.iter
                   ~f:(fun stream ->
                     (* From RFC7540§6.9.2:
                      *   An endpoint MUST treat a change to
                      *   SETTINGS_INITIAL_WINDOW_SIZE that causes any
                      *   flow-control window to exceed the maximum size as a
                      *   connection error (Section 5.4.1) of type
                      *   FLOW_CONTROL_ERROR. *)
                     if not (Scheduler.add_flow stream growth) then
                       raise Local)
                   t.streams
               with
              | () ->
                ()
              | exception Local ->
                report_connection_error
                  t
                  ~additional_debug_data:
                    (Format.sprintf
                       "Window size for stream would exceed %ld"
                       Settings.WindowSize.max_window_size)
                  Error_code.FlowControlError);
              { acc with initial_window_size = new_val }
            | MaxFrameSize x ->
              Scheduler.iter
                ~f:(fun (Stream { descriptor; _ }) ->
                  if Respd.requires_output descriptor then
                    descriptor.max_frame_size <- x)
                t.streams;
              { acc with max_frame_size = x }
            | MaxHeaderListSize x ->
              { acc with max_header_list_size = Some x })
          t.settings
          settings
      in
      t.settings <- new_settings;
      let frame_info =
        Writer.make_frame_info
          ~flags:Flags.(set_ack default_flags)
          Stream_identifier.connection
      in
      (* From RFC7540§6.5:
       *   ACK (0x1): [...] When this bit is set, the payload of the SETTINGS
       *   frame MUST be empty. *)
      Writer.write_settings t.writer frame_info [];
      t.unacked_settings <- t.unacked_settings + 1;
      Writer.wakeup t.writer
    | Error error ->
      report_error t error

let reserve_stream t { Frame.frame_header; _ } promised_stream_id headers_block =
  let { Frame.flags; stream_id; _ } = frame_header in
  (* From RFC7540§6.6:
   *   The PUSH_PROMISE frame (type=0x5) is used to notify the peer endpoint in
   *   advance of streams the sender intends to initiate. *)
  let respd =
    Stream.create
      promised_stream_id
      ~max_frame_size:t.settings.max_frame_size
      t.writer
      t.error_handler
      (on_close_stream t promised_stream_id)
  in
  (* From RFC7540§5.3.5:
   *   All streams are initially assigned a non-exclusive dependency on stream
   *   0x0. Pushed streams (Section 8.2) initially depend on their associated
   *   stream. In both cases, streams are assigned a default weight of 16. *)
  let stream : Scheduler.nonroot Scheduler.node =
    Scheduler.add
      t.streams
      ~priority:{ Priority.default_priority with stream_dependency = stream_id }
      ~initial_send_window_size:t.settings.initial_window_size
      ~initial_recv_window_size:t.config.initial_window_size
      respd
  in
  let partial_headers = create_partial_headers t flags headers_block in
  respd.state <- Reserved (PartialHeaders partial_headers);
  if not (Flags.test_end_header flags) then
    t.receiving_headers_for_stream <- Some promised_stream_id;
  handle_headers_block t stream partial_headers flags headers_block

let process_push_promise_frame
    t ({ Frame.frame_header; _ } as frame) promised_stream_id headers_block
  =
  let { Frame.stream_id; _ } = frame_header in
  (* At this point, `promised_stream_id` has already been validated by the
   * parser *)
  if not t.settings.enable_push then
    (* From RFC7540§6.6:
     *   PUSH_PROMISE MUST NOT be sent if the SETTINGS_ENABLE_PUSH setting of
     *   the peer endpoint is set to 0. An endpoint that has set this setting
     *   and has received acknowledgement MUST treat the receipt of a
     *   PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
     *   PROTOCOL_ERROR. *)
    let additional_debug_data = "Push is not enabled for the connection" in
    report_connection_error t ~additional_debug_data Error_code.ProtocolError
  else if not Stream_identifier.(promised_stream_id > t.max_pushed_stream_id)
  then
    (* From RFC7540§6.6:
     *   A receiver MUST treat the receipt of a PUSH_PROMISE that promises an
     *   illegal stream identifier (Section 5.1.1) as a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. Note that an illegal stream
     *   identifier is an identifier for a stream that is not currently in the
     *   "idle" state. *)
    let additional_debug_data =
      "Illegal stream identifier promised by PUSH_PROMISE"
    in
    report_connection_error t ~additional_debug_data Error_code.ProtocolError
  else
    let send_connection_error () =
      let additional_debug_data =
        "Received PUSH_PROMISE on a stream that is neither open nor \
         half-closed (local)"
      in
      report_connection_error t ~additional_debug_data Error_code.ProtocolError
    in
    t.max_pushed_stream_id <- promised_stream_id;
    match Scheduler.find t.streams stream_id with
    | None ->
      (* From RFC7540§6.6:
       *   A receiver MUST treat the receipt of a PUSH_PROMISE on a stream that
       *   is neither "open" nor "half-closed (local)" as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      send_connection_error ()
    | Some respd ->
      (match respd.state with
      | Active ((Open _ | HalfClosed _), _) ->
        reserve_stream t frame promised_stream_id headers_block
      | _ ->
        send_connection_error ())

let process_ping_frame t { Frame.frame_header; _ } payload =
  let { Frame.flags; _ } = frame_header in
  (* From RFC7540§6.7:
   *   ACK (0x1): When set, bit 0 indicates that this PING frame is a PING
   *   response. [...] An endpoint MUST NOT respond to PING frames containing
   *   this flag. *)
  if Flags.test_ack flags then
    match Queue.take_opt t.pending_pings with
    | Some callback ->
      callback ()
    | None ->
      (* server is ACKing a PING that we didn't send? *)
      let additional_debug_data = "Unexpected PING acknowledgement" in
      report_connection_error t ~additional_debug_data Error_code.ProtocolError
  else
    (* From RFC7540§6.7:
     *   Receivers of a PING frame that does not include an ACK flag MUST send
     *   a PING frame with the ACK flag set in response, with an identical
     *   payload. PING responses SHOULD be given higher priority than any other
     *   frame. *)
    let frame_info =
      Writer.make_frame_info
      (* From RFC7540§6.7:
       *   ACK (0x1): When set, bit 0 indicates that this PING frame is a
       *   PING response. An endpoint MUST set this flag in PING
       *   responses. *)
        ~flags:Flags.(set_ack default_flags)
        Stream_identifier.connection
    in
    (* From RFC7540§6.7:
     *   Receivers of a PING frame that does not include an ACK flag MUST send
     *   a PING frame with the ACK flag set in response, with an identical
     *   payload. *)
    Writer.write_ping t.writer frame_info payload;
    Writer.wakeup t.writer

let process_goaway_frame t _frame payload =
  let _last_stream_id, _error, debug_data = payload in
  let len = Bigstringaf.length debug_data in
  let bytes = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes debug_data ~src_off:0 bytes ~dst_off:0 ~len;
  (* TODO(anmonteiro): I think we need to allow lower numbered streams to
   * complete. *)
  shutdown_rw t

let add_window_increment
    : type a. t -> a Scheduler.PriorityTreeNode.node -> int32 -> unit
  =
 fun t stream increment ->
  let open Scheduler in
  let did_add = Scheduler.add_flow stream increment in
  let stream_id = Scheduler.stream_id stream in
  let new_flow =
    match stream with
    | Connection { flow; _ } ->
      flow
    | Stream { flow; _ } ->
      flow
  in
  if did_add then (
    if Int32.compare new_flow 0l > 0 then
      (* Don't bother waking up the writer if the new flow doesn't allow
       * the stream to write. *)
      Writer.wakeup t.writer)
  else if Stream_identifier.is_connection stream_id then
    report_connection_error
      t
      ~additional_debug_data:
        (Printf.sprintf
           "Window size for stream would exceed %ld"
           Settings.WindowSize.max_window_size)
      Error_code.FlowControlError
  else
    report_stream_error t stream_id Error_code.FlowControlError

let process_window_update_frame t { Frame.frame_header; _ } window_increment =
  let open Scheduler in
  let { Frame.stream_id; _ } = frame_header in
  (* From RFC7540§6.9:
   *   The WINDOW_UPDATE frame can be specific to a stream or to the entire
   *   connection. In the former case, the frame's stream identifier indicates
   *   the affected stream; in the latter, the value "0" indicates that the
   *   entire connection is the subject of the frame. *)
  if Stream_identifier.is_connection stream_id then
    add_window_increment t t.streams window_increment
  else
    match Scheduler.get_node t.streams stream_id with
    | Some (Stream { descriptor; _ } as stream_node) ->
      (match descriptor.state with
      | Idle ->
        (* From RFC7540§5.1:
         *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
         *   a stream in this state MUST be treated as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
        report_connection_error t Error_code.ProtocolError
      | Active _
      (* From RFC7540§5.1:
       *   reserved (local): [...] A PRIORITY or WINDOW_UPDATE frame MAY be
       *   received in this state. *)
      | Reserved _ ->
        add_window_increment t stream_node window_increment
      | Closed _ ->
        (* From RFC7540§5.1:
         *   Endpoints MUST ignore WINDOW_UPDATE or RST_STREAM frames received
         *   in this state, though endpoints MAY choose to treat frames that
         *   arrive a significant time after sending END_STREAM as a connection
         *   error (Section 5.4.1) of type PROTOCOL_ERROR. *)
        ())
    | None ->
      if not (was_closed_or_implicitly_closed t stream_id) then
        (* From RFC7540§5.1:
         *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
         *   a stream in this state MUST be treated as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
        report_connection_error t Error_code.ProtocolError

let process_continuation_frame t { Frame.frame_header; _ } headers_block =
  let { Frame.stream_id; flags; _ } = frame_header in
  match Scheduler.get_node t.streams stream_id with
  | Some (Scheduler.Stream { descriptor; _ } as stream) ->
    (match descriptor.state with
    | Active
        ( ( Open (PartialHeaders partial_headers)
          | HalfClosed (PartialHeaders partial_headers) )
        , _ ) ->
      handle_headers_block t stream partial_headers flags headers_block
    | Active
        ( ( Open (ActiveMessage { trailers_parser = Some partial_headers; _ })
          | HalfClosed
              (ActiveMessage { trailers_parser = Some partial_headers; _ }) )
        , _ ) ->
      handle_trailer_headers t stream partial_headers flags headers_block
    | _ ->
      (* TODO: maybe need to handle the case where the stream has been closed
       * due to a stream error. *)
      (* From RFC7540§6.10:
       *   A RST_STREAM is the last frame that an endpoint can send on a
       *   stream. The peer that sends the RST_STREAM frame MUST be prepared
       *   to receive any frames that were sent or enqueued for sending by
       *   the remote peer. These frames can be ignored, except where they
       *   modify connection state (such as the state maintained for header
       *   compression (Section 4.3) or flow control). *)
      report_connection_error t Error_code.ProtocolError)
  | None ->
    (* From RFC7540§6.10:
     *   A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
     *   CONTINUATION frame without the END_HEADERS flag set. A recipient that
     *   observes violation of this rule MUST respond with a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError

(* From RFC7540§1:
 *   HTTP/2 [...] allows interleaving of request and response messages on the
 *   same connection and uses an efficient coding for HTTP header fields. *)
let[@ocaml.warning "-16"] create
    ?(config = Config.default) ?push_handler ~error_handler
  =
  let push_handler =
    match push_handler with
    | Some push_handler ->
      push_handler
    | None ->
      default_push_handler
  in
  let settings =
    { (Config.to_settings config) with
      (* If the caller is not going to process PUSH_PROMISE frames, just
       * disable it. *)
      enable_push =
        config.enable_server_push && push_handler != default_push_handler
    }
  in
  let rec connection_preface_handler recv_frame settings_list =
    let t = Lazy.force t in
    (* Now process the client's SETTINGS frame. `process_settings_frame` will
     * take care of calling `wakeup_writer`. *)
    process_settings_frame t recv_frame settings_list
  and frame_handler r =
    let t = Lazy.force t in
    match r with
    | Error e ->
      report_error t e
    | Ok ({ Frame.frame_payload; frame_header } as frame) ->
      (match t.receiving_headers_for_stream with
      | Some stream_id
        when (not Stream_identifier.(stream_id === frame_header.stream_id))
             || frame_header.frame_type <> Continuation ->
        (* From RFC7540§6.2:
         *   A HEADERS frame without the END_HEADERS flag set MUST be followed
         *   by a CONTINUATION frame for the same stream. A receiver MUST treat
         *   the receipt of any other type of frame or a frame on a different
         *   stream as a connection error (Section 5.4.1) of type
         *   PROTOCOL_ERROR. *)
        report_connection_error
          t
          ~additional_debug_data:
            "HEADERS or PUSH_PROMISE without the END_HEADERS flag set must be \
             followed by a CONTINUATION frame for the same stream"
          Error_code.ProtocolError
      | _ ->
        (match frame_payload with
        | Headers (_priority, headers_block) ->
          process_headers_frame t frame headers_block
        | Data bs ->
          process_data_frame t frame bs
        | Priority priority ->
          process_priority_frame t frame priority
        | RSTStream error_code ->
          process_rst_stream_frame t frame error_code
        | Settings settings ->
          process_settings_frame t frame settings
        | PushPromise (promised_stream_id, bs) ->
          process_push_promise_frame t frame promised_stream_id bs
        | Ping data ->
          process_ping_frame t frame data
        | GoAway (last_stream_id, error, debug_data) ->
          process_goaway_frame t frame (last_stream_id, error, debug_data)
        | WindowUpdate window_size ->
          process_window_update_frame t frame window_size
        | Continuation headers_block ->
          process_continuation_frame t frame headers_block
        | Unknown _ ->
          (* From RFC7540§5.1:
           *   Frames of unknown types are ignored. *)
          ()))
  and t =
    lazy
      { settings
      ; config
        (* From RFC7540§5.1.1:
         *   Streams initiated by a client MUST use odd-numbered stream
         *   identifiers *)
      ; current_stream_id = -1l
      ; max_pushed_stream_id = 0l
      ; current_server_streams = 0
      ; receiving_headers_for_stream = None
      ; did_send_go_away = false
      ; unacked_settings = 0
      ; pending_pings = Queue.create ()
      ; error_handler
      ; push_handler
      ; reader =
          Reader.client_frames
            ~max_frame_size:settings.max_frame_size
            connection_preface_handler
            frame_handler
      ; writer = Writer.create settings.max_frame_size
      ; streams =
          Scheduler.make_root ()
          (* From RFC7540§4.3:
           *   Header compression is stateful. One compression context and one
           *   decompression context are used for the entire connection. *)
      ; hpack_encoder = Hpack.Encoder.(create settings.header_table_size)
      ; hpack_decoder = Hpack.Decoder.(create settings.header_table_size)
      }
  in
  let t = Lazy.force t in
  (* Check if the settings for the connection are different than the default
   * HTTP/2 settings. In the event that they are, we need to send a non-empty
   * SETTINGS frame advertising our configuration. *)
  let settings = Settings.settings_for_the_connection t.settings in
  (* From RFC7540§6.9.2:
   *   When an HTTP/2 connection is first established, new streams are created
   *   with an initial flow-control window size of 65,535 octets. The
   *   connection flow-control window is also 65,535 octets.
   *
   * XXX(anmonteiro): the starting setting for the initial window size for
   * _sending_ is the default of 65535 octets. We're effectively overwriting it
   * here to enforce this default after abusing the settings implementation to
   * send our (receiving) in-flow setting to the peer. Throughout other parts
   * of the code we (should) refer to it through
   * [t.config.initial_window_size]. This should probably be cleaned up in the
   * future. *)
  t.settings <-
    { t.settings with
      initial_window_size = Settings.default.initial_window_size
    };
  (* Send the client connection preface *)
  Writer.write_connection_preface t.writer settings;
  (* If a higher value for initial window size is configured, add more tokens
   * to the connection (we have no streams at this point) -- the peer is
   * allowed to send more than the defaults.
   *
   * From RFC7540§6.9.2:
   *   The connection flow-control window can only be changed using
   *   WINDOW_UPDATE frames. *)
  (if t.config.initial_window_size > Settings.default.initial_window_size then
     let diff =
       Int32.sub
         t.config.initial_window_size
         Settings.default.initial_window_size
     in
     send_window_update t t.streams diff);
  t

let create_and_add_stream t ~error_handler =
  let max_frame_size = t.settings.max_frame_size in
  t.current_stream_id <- Int32.add t.current_stream_id 2l;
  let stream_id = t.current_stream_id in
  let respd =
    Stream.create
      stream_id
      ~max_frame_size
      t.writer
      error_handler
      (on_close_stream t stream_id)
  in
  (* TODO: custom priority *)
  let _stream : Scheduler.nonroot Scheduler.node =
    Scheduler.add
      t.streams
      ~priority:Priority.default_priority
      ~initial_send_window_size:t.settings.initial_window_size
      ~initial_recv_window_size:t.config.initial_window_size
      respd
  in
  respd

(* Meant to be called after receiving an HTTP/1.1 `101 Switching_protocols`
 * response upgrading to HTTP/2. *)
let create_h2c
    ?config
    ?push_handler
    ~http_request
    ~error_handler
    (response_handler, response_error_handler)
  =
  let { Httpaf.Request.target; meth; _ } = http_request in
  match Headers.of_http1 http_request with
  | Ok headers ->
    (* From RFC7540§3.2:
     *   Upon receiving the 101 response, the client MUST send a connection
     *   preface (Section 3.5), which includes a SETTINGS frame. *)
    let t = create ?config ?push_handler ~error_handler in
    let respd = create_and_add_stream t ~error_handler:response_error_handler in
    assert (Stream_identifier.(t.current_stream_id === 1l));
    assert (Stream_identifier.(respd.id === 1l));
    let request = Request.create ~headers ~scheme:"http" meth target in
    (* From RFC7540§3.2:
     *   The HTTP/1.1 request that is sent prior to upgrade is assigned a
     *   stream identifier of 1 (see Section 5.1.1) with default priority
     *   values (Section 5.3.5). Stream 1 is implicitly "half-closed" from the
     *   client toward the server (see Section 5.1), since the request is
     *   completed as an HTTP/1.1 request. *)
    respd.state <-
      Active
        ( HalfClosed WaitingForPeer
        , { request
            (* The request body is no more than a placeholder. The HTTP/1.1
             * connection that we're upgrading from already sent it to the
             * server. Application code knows what it is.
             *
             * From RFC7540§3.2:
             *   Requests that contain a payload body MUST be sent in their
             *   entirety before the client can send HTTP/2 frames. This means
             *   that a large request can block the use of the connection until
             *   it is completely sent. *)
          ; request_body = Body.empty
          ; response_handler
          ; trailers_handler = ignore
          } );
    Writer.wakeup t.writer;
    Ok t
  | Error msg ->
    Error msg

let request
    t ?(trailers_handler = ignore) request ~error_handler ~response_handler
  =
  let max_frame_size = t.settings.max_frame_size in
  let respd = create_and_add_stream t ~error_handler in
  let request_body =
    Body.create_writer
      (Bigstringaf.create max_frame_size)
      ~ready_to_write:(fun () -> Writer.wakeup t.writer)
  in
  let frame_info =
    Writer.make_frame_info
      ~max_frame_size:t.settings.max_frame_size
      ~flags:Flags.default_flags
      respd.id
  in
  Writer.write_request_headers
    t.writer
    t.hpack_encoder
    ~priority:
      (* TODO: allow setting the priority of the request. *)
      Priority.default_priority
    frame_info
    request;
  Writer.flush t.writer (fun () ->
      respd.state <-
        Active
          ( Open WaitingForPeer
          , { request; request_body; response_handler; trailers_handler } ));
  Writer.wakeup t.writer;
  (* Closing the request body puts the stream in the half-closed (local) state.
   * This is handled by {!Respd.flush_request_body}, which transitions the
   * state once it verifies that there's no more data to send for the
   * stream. *)
  request_body

(* XXX: we store PING callbacks in FIFO order. Would it ever be the case that
 * the receipt of a PING frame acknowledges a later callback? If so, we'd need
 * to disallow sending custom PING payloads and generate a random payload that
 * we store in a Hashtbl. *)
let ping t ?payload ?(off = 0) callback =
  let payload =
    match payload with
    | None ->
      Serialize.default_ping_payload
    | Some payload ->
      if Bigstringaf.length payload - off < 8 then
        failwith "PING payload must have at least 8 octets in length";
      payload
  in
  (* From RFC7540§6.7:
   *   ACK (0x1): When set, bit 0 indicates that this PING frame is a PING
   *   response.
   *
   *   Note: this is not a PING response, quite the opposite, so we don't set
   *   the ACK flag. *)
  let frame_info = Writer.make_frame_info Stream_identifier.connection in
  Queue.add callback t.pending_pings;
  Writer.write_ping t.writer frame_info ~off payload;
  Writer.wakeup t.writer

let next_read_operation t =
  if Reader.is_closed t.reader then shutdown_reader t;
  match Reader.next t.reader with
  | (`Read | `Close) as operation ->
    operation
  | `Error e ->
    report_error t e;
    (match e with
    | ConnectionError _ ->
      (* From RFC7540§5.4.1:
       *   A connection error is any error that prevents further processing
       *   of the frame layer or corrupts any connection state. *)
      `Close
    | StreamError _ ->
      (* From RFC7540§5.4.2:
       *   A stream error is an error related to a specific stream that does
       *   not affect processing of other streams. *)
      `Read)

let read t bs ~off ~len = Reader.read_with_more t.reader bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  Reader.read_with_more t.reader bs ~off ~len Complete

(* XXX(anmonteiro): this function is here to please the Gluten `RUNTIME`
 * interface.
 *
 * We don't expect this function to ever be called. H2 never issues `Yield`
 * commands because the connection is multiplexed, and it's therefore always
 * looking to read frames from the peer. *)
let yield_reader _t k = k ()

let next_write_operation t =
  flush_request_body t;
  Writer.next t.writer

let yield_writer t k = Writer.on_wakeup_writer t.writer k

let report_write_result t result = Writer.report_result t.writer result
