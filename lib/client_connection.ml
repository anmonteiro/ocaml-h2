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
module Scheduler = Scheduler.Client_scheduler

module Queue = struct
  include Queue

  let take_opt t =
    match is_empty t with true -> None | false -> Some (take t)
end

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Protocol_error
  | `Exn of exn
  ]

type response_handler = Response.t -> [ `read ] Body.t -> unit

(* TODO: we'll need a connection-level error handler that terminates the
 * connection and one for each individual stream. *)
type error_handler = error -> unit

type reader_state =
  | New of Reader.server_connection_preface
  | Active of Reader.frame

type t =
  { settings : Settings.t
  ; mutable reader : reader_state
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
  ; wakeup_writer : (unit -> unit) ref
        (* From RFC7540§4.3:
         *   Header compression is stateful. One compression context and one
         *   decompression context are used for the entire connection. *)
  ; hpack_encoder : Hpack.Encoder.t
  ; hpack_decoder : Hpack.Decoder.t
  }

let default_push_handler = Sys.opaque_identity (fun _ -> Ok (fun _ _ -> ()))

let is_active t = match t.reader with Active _ -> true | New _ -> false

let reader t =
  match t.reader with New reader -> reader | Active reader -> reader

let is_shutdown t = Reader.is_closed (reader t) && Writer.is_closed t.writer

let is_closed t = Reader.is_closed (reader t) && Writer.is_closed t.writer

let on_wakeup_writer t k =
  if is_shutdown t then
    failwith "on_wakeup_writer on closed conn"
  else
    t.wakeup_writer := k

let default_wakeup_writer () = ()

let _wakeup_writer wakeup_ref =
  let f = !wakeup_ref in
  wakeup_ref := default_wakeup_writer;
  f ()

let wakeup_writer t = _wakeup_writer t.wakeup_writer

let shutdown_reader t = Reader.force_close (reader t)

let flush_request_body t =
  if is_active t then
    Scheduler.flush t.streams

let shutdown_writer t =
  flush_request_body t;
  Writer.close t.writer

let shutdown t =
  shutdown_reader t;
  shutdown_writer t

let handle_error t = function
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
        if String.length data == 0 then
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
      t.error_handler `Protocol_error;
      Writer.flush t.writer (fun () ->
          (* XXX: We need to allow lower numbered streams to complete before
           * shutting down. *)
          shutdown t);
      t.did_send_go_away <- true;
      wakeup_writer t)
  | StreamError (stream_id, error) ->
    (* TODO: This branch probably needs to report the error to the stream error
     * handler, and additionally deliver EOF to the response body. Should not
     * forget to handle streams in the Reserved state (for which there is no
     * error handler). *)
    (match Scheduler.find t.streams stream_id with
    | Some respd ->
      Respd.report_error respd `Protocol_error error
    | None ->
      (* Possible if the stream was going to enter the Idle state (first time
       * we saw e.g. a PRIORITY frame for it) but had e.g. a
       * FRAME_SIZE_ERROR. *)
      let frame_info = Writer.make_frame_info stream_id in
      Writer.write_rst_stream t.writer frame_info error);
    wakeup_writer t

let report_connection_error t ?(additional_debug_data = "") error =
  handle_error t (ConnectionError (error, additional_debug_data))

let report_stream_error t stream_id error =
  handle_error t (StreamError (stream_id, error))

let set_error_and_handle t stream error error_code =
  Respd.report_error stream error error_code;
  wakeup_writer t

let report_exn t exn =
  if not (is_shutdown t) then
    let additional_debug_data = Printexc.to_string exn in
    report_connection_error t ~additional_debug_data Error.InternalError

let handle_push_promise_headers t respd headers =
  (* From RFC7540§8.2.2:
   *   The header fields in PUSH_PROMISE and any subsequent CONTINUATION frames
   *   MUST be a valid and complete set of request header fields (Section
   *   8.1.2.3). *)
  match Headers.method_path_and_scheme_or_malformed headers with
  | None ->
    (* From RFC7540§8.2.2:
     *   If a client receives a PUSH_PROMISE that does not include a complete
     *   and valid set of header fields [...] it MUST respond with a stream
     *   error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t respd.Stream.id Error.ProtocolError
  | Some (meth, path, scheme) ->
    let meth = Httpaf.Method.of_string meth in
    (match
       ( meth
       , Headers.get_pseudo headers "authority"
       , Message.body_length headers )
     with
    | (#Httpaf.Method.standard as meth), _, _
      when not Httpaf.Method.(is_cacheable meth && is_safe meth) ->
      report_stream_error t respd.id Error.ProtocolError
    | _, _, `Fixed len when Int64.compare len 0L != 0 ->
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
      report_stream_error t respd.id Error.ProtocolError
    (* From RFC7540§8.2:
     *   The server MUST include a value in the :authority pseudo-header field
     *   for which the server is authoritative (see Section 10.1). A client
     *   MUST treat a PUSH_PROMISE for which the server is not authoritative as
     *   a stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    | _, None, _ | _, _, `Error _ ->
      report_stream_error t respd.id Error.ProtocolError
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
        respd.state
        <- Active
             ( HalfClosed Stream.WaitingForPeer
             , { Respd.request; request_body; response_handler } )
      | Error _ ->
        (* From RFC7540§6.6:
         *   Recipients of PUSH_PROMISE frames can choose to reject promised
         *   streams by returning a RST_STREAM referencing the promised stream
         *   identifier back to the sender of the PUSH_PROMISE. *)
        Stream.reset_stream respd Error.Cancel))

let handle_response_headers t respd ~end_stream active_request headers =
  (* From RFC7540§8.1.2.6:
   *   Clients MUST NOT accept a malformed response.
   *
   * Note: in the branches where a malformed response is detected, the response
   * handler is not called. *)
  match Headers.get_multi_pseudo headers "status" with
  | [ status ] ->
    let response = Response.create ~headers (Status.of_string status) in
    (match end_stream, Message.body_length headers with
    | true, `Fixed len when Int64.compare len 0L != 0 ->
      (* From RFC7540§8.1.2.6:
       *   A request or response is also malformed if the value of a
       *   content-length header field does not equal the sum of the DATA
       *   frame payload lengths that form the body. *)
      set_error_and_handle
        t
        respd
        (`Invalid_response_body_length response)
        ProtocolError
    | _, `Error _ ->
      set_error_and_handle
        t
        respd
        (`Invalid_response_body_length response)
        ProtocolError
    | _, body_length ->
      let response_body =
        if end_stream then
          Body.empty
        else
          match body_length with
          | `Fixed n ->
            Body.create (Bigstringaf.create (Int64.to_int n))
          | `Error _ | `Unknown ->
            (* Not sure how much data we're gonna get. Delay the allocation
             * until we get a data frame. *)
            Body.create Bigstringaf.empty
      in
      let new_response_state =
        Respd.create_active_response response response_body
      in
      respd.state
      <- Stream.(
           Active
             ( (if is_open respd then
                  Open new_response_state
               else
                 HalfClosed new_response_state)
             , active_request ));
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

let handle_headers t ~end_stream respd headers =
  (* From RFC7540§5.1.2:
   *   Endpoints MUST NOT exceed the limit set by their peer. An endpoint that
   *   receives a HEADERS frame that causes its advertised concurrent stream
   *   limit to be exceeded MUST treat this as a stream error (Section 5.4.2)
   *   of type PROTOCOL_ERROR or REFUSED_STREAM. *)
  if t.current_server_streams + 1 > t.settings.max_concurrent_streams then
    if t.unacked_settings > 0 then
      (* From RFC7540§8.1.4:
       *   The REFUSED_STREAM error code can be included in a RST_STREAM frame
       *   to indicate that the stream is being closed prior to any processing
       *   having occurred. Any request that was sent on the reset stream can
       *   be safely retried.
       *
       * Note: if there are pending SETTINGS to acknowledge, assume there was a
       * race condition and let the client retry. *)
      report_stream_error t respd.Stream.id Error.RefusedStream
    else
      report_stream_error t respd.Stream.id Error.ProtocolError
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
        respd.state <- Active (Stream.(HalfClosed FullHeaders), active_request));
      handle_response_headers t respd ~end_stream active_request headers
    | _ ->
      (* Unreachable. This function is only invoked if the stream is active. *)
      assert false)

let handle_headers_block
    t ?(is_trailers = false) respd partial_headers flags headers_block
  =
  let open AB in
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
        handle_headers t ~end_stream respd headers
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
        set_error_and_handle
          t
          respd
          (`Malformed_response message)
          ProtocolError
    (* From RFC7540§4.3:
     *   A decoding error in a header block MUST be treated as a connection
     *   error (Section 5.4.1) of type COMPRESSION_ERROR. *)
    | Done (_, Error _) | Partial _ ->
      report_connection_error t Error.CompressionError
    | Fail (_, _, message) ->
      report_connection_error
        t
        ~additional_debug_data:message
        Error.CompressionError)
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

(* TODO: reprioritize stream? *)
let handle_first_response_bytes
    t respd active_request frame_header ?priority:_ headers_block
  =
  let { Frame.flags; stream_id; _ } = frame_header in
  let partial_headers = create_partial_headers t flags headers_block in
  let remote_state = Stream.PartialHeaders partial_headers in
  respd.Stream.state
  <- (if Stream.is_open respd then
        Active (Open remote_state, active_request)
     else
       Active (HalfClosed remote_state, active_request));
  if not (Flags.test_end_header flags) then
    t.receiving_headers_for_stream <- Some stream_id;
  handle_headers_block t respd partial_headers flags headers_block

let process_trailer_headers t respd active_response frame_header headers_block =
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
    handle_trailer_headers t respd partial_headers flags headers_block

let process_headers_frame t { Frame.frame_header; _ } ?priority headers_block =
  let { Frame.stream_id; _ } = frame_header in
  match priority with
  | Some { Priority.stream_dependency; _ }
    when Stream_identifier.(stream_dependency === stream_id) ->
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a
     *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error.ProtocolError
  | _ ->
    (match Scheduler.find t.streams stream_id with
    | None ->
      (* TODO: What does it mean to receive a response for a stream that's no
       * longer in the priority tree? Either:
       * 1. we canceled a request and the response was already in flight.
       * 2. we're getting a response for a request we didn't send?! *)
      assert false
    | Some respd ->
      (match respd.state with
      | Idle ->
        (* From RFC7540§6.2:
         *   HEADERS frames can be sent on a stream in the "idle", "reserved
         *   (local)", "open", or "half-closed (remote)" state. *)
        report_connection_error t Error.ProtocolError
      | Active
          ((Open WaitingForPeer | HalfClosed WaitingForPeer), active_request)
        ->
        handle_first_response_bytes
          t
          respd
          active_request
          frame_header
          ?priority
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
          respd
          active_response
          frame_header
          headers_block
      | Closed { reason = ResetByThem _; _ } ->
        (* From RFC7540§5.1:
         *   closed: [...] An endpoint that receives any frame other than
         *   PRIORITY after receiving a RST_STREAM MUST treat that as a
         *   stream error (Section 5.4.2) of type STREAM_CLOSED. *)
        report_stream_error t stream_id Error.StreamClosed
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
        report_connection_error t Error.StreamClosed))

let send_window_update : type a.
    t -> a Scheduler.PriorityTreeNode.node -> int -> unit
  =
 fun t stream n ->
  let send_window_update_frame stream_id n =
    let valid_inflow = Scheduler.add_inflow stream n in
    assert valid_inflow;
    let frame_info = Writer.make_frame_info stream_id in
    Writer.write_window_update t.writer frame_info n
  in
  if n > 0 then (
    let max_window_size = Settings.WindowSize.max_window_size in
    let stream_id = Scheduler.stream_id stream in
    let rec loop n =
      if n > max_window_size then (
        send_window_update_frame stream_id max_window_size;
        loop (n - max_window_size))
      else
        send_window_update_frame stream_id n
    in
    loop n;
    wakeup_writer t)

let process_data_frame t { Frame.frame_header; _ } bstr =
  let open Scheduler in
  let { Frame.flags; stream_id; payload_length; _ } = frame_header in
  (* From RFC7540§6.9:
   *   A receiver that receives a flow-controlled frame MUST always account
   *   for its contribution against the connection flow-control window,
   *   unless the receiver treats this as a connection error (Section 5.4.1).
   *   This is necessary even if the frame is in error. *)
  Scheduler.deduct_inflow t.streams payload_length;
  match Scheduler.get_node t.streams stream_id with
  | Some (Stream { descriptor; _ } as stream) ->
    (match descriptor.Stream.state with
    | Active
        ( ( Open (ActiveMessage response_info)
          | HalfClosed (ActiveMessage response_info) )
        , _ ) ->
      let { Respd.response; response_body; response_body_bytes; _ } =
        response_info
      in
      response_info.response_body_bytes
      <- Int64.(add response_body_bytes (of_int (Bigstringaf.length bstr)));
      if not Scheduler.(allowed_to_receive t.streams stream payload_length)
      then
        (* From RFC7540§6.9:
         *  A receiver MAY respond with a stream error (Section 5.4.2) or
         *  connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR if it
         *  is unable to accept a frame. *)
        report_stream_error t stream_id Error.FlowControlError
      else (
        Scheduler.deduct_inflow stream payload_length;
        match Message.body_length response.headers with
        | `Fixed len
        (* Getting more than the server declared *)
          when Int64.compare response_info.response_body_bytes len > 0 ->
          (* Give back connection-level flow-controlled bytes (we use payload
           * length to include any padding bytes that the frame might have
           * included - which were ignored at parse time). *)
          send_window_update t t.streams payload_length;
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
          (* XXX(anmonteiro): should we only give back flow control after we
           * delivered EOF to the response body? There's a potential flow
           * control issue right now where we're handing out connection-level
           * flow control tokens on the receipt of every DATA frame. This
           * might allow servers to send an unbounded number of bytes. Same
           * issue on the server (see corresponding comment). *)
          (* From RFC7540§6.9.1:
           *   The receiver of a frame sends a WINDOW_UPDATE frame as it
           *   consumes data and frees up space in flow-control windows.
           *   Separate WINDOW_UPDATE frames are sent for the stream- and
           *   connection-level flow-control windows. *)
          send_window_update t t.streams payload_length;
          send_window_update t stream payload_length;
          let faraday = Body.unsafe_faraday response_body in
          if not (Faraday.is_closed faraday) then (
            Faraday.schedule_bigstring faraday bstr;
            if end_stream then Body.close_reader response_body);
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
      report_connection_error t Error.ProtocolError
    (* This is technically in the half-closed (local) state *)
    | Closed { reason = ResetByUs NoError; _ } ->
      (* From RFC7540§6.9:
       *   A receiver that receives a flow-controlled frame MUST always
       *   account for its contribution against the connection flow-control
       *   window, unless the receiver treats this as a connection error
       *   (Section 5.4.1). This is necessary even if the frame is in
       *   error. *)
      send_window_update t t.streams payload_length
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
      send_window_update t t.streams payload_length;
      (* From RFC7540§6.1:
       *   If a DATA frame is received whose stream is not in "open" or
       *   "half-closed (local)" state, the recipient MUST respond with a
       *   stream error (Section 5.4.2) of type STREAM_CLOSED. *)
      report_stream_error t stream_id Error.StreamClosed)
  | None ->
    (* From RFC7540§5.1:
     *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
     *   a stream in this state MUST be treated as a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError

let on_stream_closed t () =
  t.current_server_streams <- t.current_server_streams - 1

let process_priority_frame t { Frame.frame_header; _ } priority =
  let { Frame.stream_id; _ } = frame_header in
  let { Priority.stream_dependency; _ } = priority in
  if Stream_identifier.(stream_id === stream_dependency) then
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a
     *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error.ProtocolError
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
    (match respd.state with
    | Idle ->
      (* From RFC7540§6.4:
       *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
       *   state. If a RST_STREAM frame identifying an idle stream is
       *   received, the recipient MUST treat this as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error.ProtocolError
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
      Stream.finish_stream respd (ResetByThem error_code))
  | None ->
    (* We might have removed the stream from the hash table. If its stream
     * id is strictly smaller than the max client stream id we've seen, then
     * it must have been closed. *)
    if Stream_identifier.(stream_id >= t.current_stream_id) then
      (* From RFC7540§6.4:
       *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
       *   state. If a RST_STREAM frame identifying an idle stream is
       *   received, the recipient MUST treat this as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR.
       *
       * Note:
       *   If we didn't find the stream in the hash table it must be "idle". *)
      report_connection_error t Error.ProtocolError

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
      report_connection_error t ~additional_debug_data Error.ProtocolError)
  else
    match Settings.check_settings_list ~is_client:true settings with
    | None ->
      (* From RFC7540§6.5:
       *   Each parameter in a SETTINGS frame replaces any existing value for
       *   that parameter. Parameters are processed in the order in which they
       *   appear, and a receiver of a SETTINGS frame does not need to maintain
       *   any state other than the current value of its parameters. *)
      List.iter
        (function
          | Settings.HeaderTableSize, x ->
            (* From RFC7540§6.5.2:
             *   Allows the sender to inform the remote endpoint of the maximum
             *   size of the header compression table used to decode header
             *   blocks, in octets. *)
            t.settings.header_table_size <- x;
            Hpack.Encoder.set_capacity t.hpack_encoder x
          | EnablePush, x ->
            (* We've already verified that this setting is either 0 or 1 in the
             * call to `Settings.check_settings_list` above. *)
            t.settings.enable_push <- x == 1
          | MaxConcurrentStreams, x ->
            t.settings.max_concurrent_streams <- x
          | InitialWindowSize, new_val ->
            (* From RFC7540§6.9.2:
             *   [...] a SETTINGS frame can alter the initial flow-control
             *   window size for streams with active flow-control windows (that
             *   is, streams in the "open" or "half-closed (remote)" state).
             *   When the value of SETTINGS_INITIAL_WINDOW_SIZE changes, a
             *   receiver MUST adjust the size of all stream flow-control
             *   windows that it maintains by the difference between the new
             *   pvalue and the old value. *)
            let old_val = t.settings.initial_window_size in
            t.settings.initial_window_size <- new_val;
            let growth = new_val - old_val in
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
                     "Window size for stream would exceed %d"
                     Settings.WindowSize.max_window_size)
                Error.FlowControlError)
          | MaxFrameSize, x ->
            t.settings.max_frame_size <- x;
            Scheduler.iter
              ~f:(fun (Stream { descriptor; _ }) ->
                if Respd.requires_output descriptor then
                  descriptor.max_frame_size <- x)
              t.streams
          | MaxHeaderListSize, x ->
            t.settings.max_header_list_size <- Some x)
        settings;
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
      wakeup_writer t
    | Some error ->
      handle_error t error

let reserve_stream t { Frame.frame_header; _ } promised_stream_id headers_block
  =
  let { Frame.flags; _ } = frame_header in
  (* From RFC7540§6.6:
   *   The PUSH_PROMISE frame (type=0x5) is used to notify the peer endpoint in
   *   advance of streams the sender intends to initiate. *)
  let respd =
    Stream.create
      promised_stream_id
      ~max_frame_size:t.settings.max_frame_size
      t.writer
      t.error_handler
      (on_stream_closed t)
  in
  Scheduler.add
    t.streams
    ~initial_window_size:t.settings.initial_window_size
    respd;
  let partial_headers = create_partial_headers t flags headers_block in
  respd.state <- Reserved (PartialHeaders partial_headers);
  if not (Flags.test_end_header flags) then
    t.receiving_headers_for_stream <- Some promised_stream_id;
  handle_headers_block t respd partial_headers flags headers_block

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
    report_connection_error t ~additional_debug_data Error.ProtocolError
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
    report_connection_error t ~additional_debug_data Error.ProtocolError
  else
    let send_connection_error () =
      let additional_debug_data =
        "Received PUSH_PROMISE on a stream that is neither open nor \
         half-closed (local)"
      in
      report_connection_error t ~additional_debug_data Error.ProtocolError
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
      report_connection_error t ~additional_debug_data Error.ProtocolError
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
    wakeup_writer t

let process_goaway_frame t _frame payload =
  let _last_stream_id, _error, debug_data = payload in
  let len = Bigstringaf.length debug_data in
  let bytes = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes debug_data ~src_off:0 bytes ~dst_off:0 ~len;
  (* TODO(anmonteiro): I think we need to allow lower numbered streams to
   * complete. *)
  shutdown t

let add_window_increment : type a.
    t -> a Scheduler.PriorityTreeNode.node -> int -> unit
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
    if new_flow > 0 then
      (* Don't bother waking up the writer if the new flow doesn't allow
       * the stream to write. *)
      wakeup_writer t)
  else if Stream_identifier.is_connection stream_id then
    report_connection_error
      t
      ~additional_debug_data:
        (Printf.sprintf
           "Window size for stream would exceed %d"
           Settings.WindowSize.max_window_size)
      Error.FlowControlError
  else
    report_stream_error t stream_id Error.FlowControlError

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
        report_connection_error t Error.ProtocolError
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
      (* From RFC7540§5.1:
       *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
       *   a stream in this state MUST be treated as a connection error
       *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error.ProtocolError

let process_continuation_frame t { Frame.frame_header; _ } headers_block =
  let { Frame.stream_id; flags; _ } = frame_header in
  match Scheduler.find t.streams stream_id with
  | Some stream ->
    (match stream.Stream.state with
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
      report_connection_error t Error.ProtocolError)
  | None ->
    (* From RFC7540§6.10:
     *   A CONTINUATION frame MUST be preceded by a HEADERS, PUSH_PROMISE or
     *   CONTINUATION frame without the END_HEADERS flag set. A recipient that
     *   observes violation of this rule MUST respond with a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError

(* Unlike e.g. http/af's current Client implementation (Oneshot) where a new
 * connection is created per request, we create a single connection where all
 * requests go through. HTTP/2 allows concurrency to exist on the connection
 * level.
 *
 * From RFC7540§1:
 *   HTTP/2 [...] allows interleaving of request and response messages on the
 *   same connection and uses an efficient coding for HTTP header fields. *)
let create ?(config = Config.default) ?push_handler ~error_handler =
  let push_handler =
    match push_handler with
    | Some push_handler ->
      push_handler
    | None ->
      default_push_handler
  in
  let settings =
    { Settings.default_settings with
      max_frame_size = config.read_buffer_size
    ; max_concurrent_streams = config.max_concurrent_streams
    ; initial_window_size = config.initial_window_size
    ; enable_push =
        (* If the caller is not going to process PUSH_PROMISE frames, just
         * disable it. *)
        config.enable_server_push && push_handler != default_push_handler
    }
  in
  let rec preface_handler recv_frame settings_list =
    let t = Lazy.force t in
    (* Now process the client's SETTINGS frame. `process_settings_frame` will
     * take care of calling `wakeup_writer`. *)
    process_settings_frame t recv_frame settings_list
  and frame_handler t = function
    | Error e ->
      handle_error t e
    | Ok ({ Frame.frame_payload; frame_header } as frame) ->
      (match t.receiving_headers_for_stream with
      | Some stream_id
        when (not Stream_identifier.(stream_id === frame_header.stream_id))
             || frame_header.frame_type != Continuation ->
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
          Error.ProtocolError
      | _ ->
        (match frame_payload with
        | Headers (priority, headers_block) ->
          process_headers_frame t frame ?priority headers_block
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
          New (Reader.server_connection_preface preface_handler)
          (* TODO: `response_buffer_size`?! *)
      ; writer = Writer.create Config.default.response_buffer_size
      ; streams = Scheduler.make_root ()
      ; wakeup_writer =
          ref default_wakeup_writer
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
  (* Send the client connection preface *)
  Writer.write_connection_preface t.writer settings;
  (* We can start reading frames once the connection preface has been written
   * to the wire. *)
  Writer.flush t.writer (fun () ->
      t.reader <- Active (Reader.frame (frame_handler t)));
  (* If a higher value for initial window size is configured, add more
   * tokens to the connection (we have no streams at this point). *)
  (if
   t.settings.initial_window_size
   > Settings.default_settings.initial_window_size
 then
     let diff =
       t.settings.initial_window_size
       - Settings.default_settings.initial_window_size
     in
     send_window_update t t.streams diff);
  t

let request t request ~error_handler ~response_handler =
  let max_frame_size = t.settings.max_frame_size in
  let request_body = Body.create (Bigstringaf.create max_frame_size) in
  t.current_stream_id <- Int32.add t.current_stream_id 2l;
  let stream_id = t.current_stream_id in
  let respd =
    Stream.create
      stream_id
      ~max_frame_size
      t.writer
      error_handler
      (on_stream_closed t)
  in
  (* TODO: priority *)
  Scheduler.add
    t.streams (* ?priority *)
    ~initial_window_size:t.settings.initial_window_size
    respd;
  let frame_info =
    Writer.make_frame_info ~max_frame_size ~flags:Flags.default_flags stream_id
  in
  Writer.write_request_headers t.writer t.hpack_encoder frame_info request;
  Writer.flush t.writer (fun () ->
      respd.state
      <- Active
           (Open WaitingForPeer, { request; request_body; response_handler }));
  wakeup_writer t;
  (* TODO: closing the request body puts the stream on half-closed (local)
   * state *)
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
  wakeup_writer t

let next_read_operation t =
  if Reader.is_closed (reader t) then shutdown_reader t;
  match Reader.next (reader t) with
  | (`Read | `Close) as operation ->
    operation
  | `Error e ->
    (match t.reader with
    | New _ ->
      (* From RFC7540§5.4.1:
       *   Clients and servers MUST treat an invalid connection preface as a
       *   connection error (Section 5.4.1) of type PROTOCOL_ERROR. A GOAWAY
       *   frame (Section 6.8) MAY be omitted in this case, since an invalid
       *   preface indicates that the peer is not using HTTP/2. *)
      report_connection_error
        t
        ~additional_debug_data:"Invalid connection preface"
        Error.ProtocolError;
      `Close
    | Active _ ->
      handle_error t e;
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
        `Read))

let read t bs ~off ~len =
  Reader.read_with_more (reader t) bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  Reader.read_with_more (reader t) bs ~off ~len Complete

let next_write_operation t =
  flush_request_body t;
  Writer.next t.writer

let yield_writer t k =
  if is_active t then (
    Scheduler.on_more_output_available
      t.streams
      (* FIXME *)
      (t.current_stream_id, 0l)
      k;
    (* We need to let both the connection and each individual streams wake up
     * the connection, given that some control frames can be scheduled to be
     * written when all the streams are yielding. *)
    on_wakeup_writer t k)
  else if Writer.is_closed t.writer then
    k ()
  else
    on_wakeup_writer t k

let report_write_result t result = Writer.report_result t.writer result
