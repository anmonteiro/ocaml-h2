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
module Streams = Streams.Server_streams

type request_handler = Reqd.t -> unit

type error =
  [ `Bad_request
  | `Internal_server_error
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [ `write ] Body.t) -> unit

type reader_state =
  | New of Reader.client_connection_preface
  | Active of Reader.frame

type t =
  { settings : Settings.t
  ; mutable reader : reader_state
  ; writer : Writer.t
  ; config : Config.t
  ; request_handler : request_handler
  ; error_handler : error_handler
  ; streams : Streams.t
        (* Number of currently open client streams. Used for MAX_CONCURRENT_STREAMS
         * bookkeeping *)
  ; mutable current_client_streams : int
  ; mutable max_client_stream_id : Stream_identifier.t
  ; mutable max_pushed_stream_id : Stream_identifier.t
  ; mutable receiving_headers_for_stream : Stream_identifier.t option
  ; mutable did_send_go_away : bool
  ; wakeup_writer : (unit -> unit) ref
        (* From RFC7540§4.3:
         *   Header compression is stateful. One compression context and one
         *   decompression context are used for the entire connection. *)
  ; hpack_encoder : Hpack.Encoder.t
  ; hpack_decoder : Hpack.Decoder.t
  }

let reader t =
  match t.reader with New reader -> reader | Active reader -> reader

let is_closed t = Reader.is_closed (reader t) && Writer.is_closed t.writer

let is_shutdown t = Reader.is_closed (reader t) && Writer.is_closed t.writer

let is_active t = match t.reader with Active _ -> true | New _ -> false

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

(* if is_active t then Reqd.close_request_body (current_reqd_exn t) else
   wakeup_reader t *)

let shutdown_writer t =
  Writer.close t.writer;
  ()

(* if is_active t then Reqd.close_request_body (current_reqd_exn t) else
   wakeup_writer t *)

(* let error_code t = if is_active t then Reqd.error_code (current_reqd_exn t)
   else None *)

let shutdown t =
  shutdown_reader t;
  shutdown_writer t;
  wakeup_writer t

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
        ~last_stream_id:t.max_client_stream_id
        error;
      Writer.flush t.writer (fun () ->
          (* XXX: We need to allow lower numbered streams to complete before
           * shutting down. *)
          shutdown t);
      t.did_send_go_away <- true;
      wakeup_writer t)
  | StreamError (stream_id, error) ->
    (match Streams.find t.streams stream_id with
    | Some reqd ->
      Reqd.reset_stream reqd error
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

let set_error_and_handle ?request t stream error error_code =
  assert (is_active t);
  assert (request = None);
  Reqd.report_error stream error error_code;
  wakeup_writer t

let report_exn t exn =
  if not (is_shutdown t) then
    let additional_debug_data = Printexc.to_string exn in
    report_connection_error t ~additional_debug_data Error.InternalError

let on_stream_closed t () =
  t.current_client_streams <- t.current_client_streams - 1

let create_push_stream ({ max_pushed_stream_id; _ } as t) () =
  if not t.settings.enable_push then
    (* From RFC7540§6.6:
     *   PUSH_PROMISE MUST NOT be sent if the SETTINGS_ENABLE_PUSH setting of
     *   the peer endpoint is set to 0. *)
    Error "PUSH not enabled"
  else if
    Stream_identifier.(Int32.(add t.max_pushed_stream_id 2l) > max_stream_id)
  then (
    (* From RFC7540§5.1:
     *   Stream identifiers cannot be reused. Long-lived connections can result
     *   in an endpoint exhausting the available range of stream identifiers.
     *   [...] A server that is unable to establish a new stream identifier can
     *   send a GOAWAY frame so that the client is forced to open a new
     *   connection for new streams. *)
    report_connection_error t Error.NoError;
    Error "Stream identifiers exhausted")
  else
    let pushed_stream_id = Int32.add max_pushed_stream_id 2l in
    t.max_pushed_stream_id <- pushed_stream_id;
    let reqd =
      Reqd.create
        pushed_stream_id
        ~max_frame_size:t.settings.max_frame_size
        t.writer
        t.error_handler
        (on_stream_closed t)
    in
    Streams.add
      t.streams (* TODO: *)
                (* ?priority *)
      ~initial_window_size:t.settings.initial_window_size
      reqd;
    Ok (reqd, fun () -> wakeup_writer t)

let method_path_and_scheme_or_malformed headers =
  match
    Headers.(
      ( get_multi_pseudo headers "method"
      , get_multi_pseudo headers "scheme"
      , get_multi_pseudo headers "path" ))
  with
  | _, [ ("http" | "https") ], [ path ] when String.length path == 0 ->
    (* From RFC7540§8.1.2.6:
     *   This pseudo-header field MUST NOT be empty for http or https URIs;
     *   http or https URIs that do not contain a path component MUST include a
     *   value of '/'. *)
    None
  (* From RFC7540§8.1.2.3:
   *   All HTTP/2 requests MUST include exactly one valid value for the
   *   :method, :scheme, and :path pseudo-header fields, unless it is a
   *   CONNECT request (Section 8.3). *)
  (* TODO: handle CONNECT requests *)
  | [ meth ], [ scheme ], [ path ] ->
    if Headers.valid_request_headers headers then
      Some (meth, path, scheme)
    else
      None
  | _ ->
    None

let handle_headers t ~end_stream reqd headers =
  (* From RFC7540§5.1.2:
   *   Endpoints MUST NOT exceed the limit set by their peer. An endpoint that
   *   receives a HEADERS frame that causes its advertised concurrent stream
   *   limit to be exceeded MUST treat this as a stream error (Section 5.4.2)
   *   of type PROTOCOL_ERROR or REFUSED_STREAM. *)
  if t.current_client_streams + 1 > t.settings.max_concurrent_streams then
    (* From RFC7540§8.1.4:
     *   The REFUSED_STREAM error code can be included in a RST_STREAM frame to
     *   indicate that the stream is being closed prior to any processing
     *   having occurred. Any request that was sent on the reset stream can be
     *   safely retried. *)
    report_stream_error t reqd.Reqd.id Error.RefusedStream
  else
    (* From RFC7540§5.1.2:
     *   Streams that are in the "open" state or in either of the "half-closed"
     *   states count toward the maximum number of streams that an endpoint is
     *   permitted to open. *)
    let active_stream =
      Reqd.create_active_stream
        t.hpack_encoder
        t.config.response_body_buffer_size
        (create_push_stream t)
    in
    reqd.state <- Open (FullHeaders active_stream);
    t.current_client_streams <- t.current_client_streams + 1;
    match method_path_and_scheme_or_malformed headers with
    | None ->
      (* From RFC7540§8.1.2.6:
       *   For malformed requests, a server MAY send an HTTP response prior to
       *   closing or resetting the stream. *)
      set_error_and_handle t reqd `Bad_request ProtocolError
    | Some (meth, path, scheme) ->
      (match end_stream, Message.unique_content_length_values headers with
      | true, [ content_length ]
        when Int64.compare (Message.content_length_of_string content_length) 0L
             != 0 ->
        (* From RFC7540§8.1.2.6:
         *   A request or response is also malformed if the value of a
         *   content-length header field does not equal the sum of the DATA
         *   frame payload lengths that form the body. *)
        set_error_and_handle t reqd `Bad_request ProtocolError
      | _ ->
        let request =
          Request.create ~scheme ~headers (Httpaf.Method.of_string meth) path
        in
        let request_body =
          if end_stream then
            Body.empty
          else
            (* TODO: Initializing the request body with an empty bigstring trades
             * an allocation here vs. when the first data frame arrives. This
             * would probably make sense if this were HTTP/1 because we wouldn't
             * know whether the request had a body or not, but in HTTP/2 we
             * clearly do. It's probably fine to just allocate here if we know
             * we're gonna get data. *)
            Body.create Bigstringaf.empty
        in
        let new_stream_state =
          Reqd.create_active_request active_stream request request_body
        in
        if end_stream then (
          (* From RFC7540§5.1:
           *   [...] an endpoint receiving an END_STREAM flag causes the stream
           *   state to become "half-closed (remote)". *)
          reqd.state <- HalfClosedRemote new_stream_state;
          (* Deliver EOF to the request body, as the handler might be waiting
           * on it to produce a response. *)
          Body.close_reader request_body)
        else
          reqd.state <- Open (ActiveRequest new_stream_state);
        t.request_handler reqd;
        wakeup_writer t)

let handle_headers_block
    t ?(is_trailers = false) reqd partial_headers flags headers_block
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
      if not is_trailers then (
        (* Note:
         *   the highest stream identifier that the server has seen is set here
         *   (as opposed to when the stream was first opened - when handling
         *   the first HEADERS frame) because it refers to the highest stream
         *   identifier that the server will process.
         *
         * From RFC7540§6.8:
         *   The last stream identifier in the GOAWAY frame contains the
         *   highest-numbered stream identifier for which the sender of the
         *   GOAWAY frame might have taken some action on or might yet take
         *   action on. All streams up to and including the identified stream
         *   might have been processed in some way. *)
        t.max_client_stream_id <- reqd.Reqd.id;
        (* `handle_headers` will take care of transitioning the stream state *)
        handle_headers t ~end_stream:partial_headers.end_stream reqd headers)
      else if Headers.trailers_valid headers then (
        Reqd.deliver_trailer_headers reqd headers;
        let request_body = Reqd.request_body reqd in
        Body.close_reader request_body)
      else
        (* From RFC7540§8.1.2.1:
         *   Pseudo-header fields MUST NOT appear in trailers. Endpoints MUST
         *   treat a request or response that contains undefined or invalid
         *   pseudo-header fields as malformed (Section 8.1.2.6). *)
        set_error_and_handle t reqd `Bad_request ProtocolError
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

let open_stream t frame_header ?priority headers_block =
  let open Streams in
  let { Frame.flags; stream_id; _ } = frame_header in
  if not (Int32.(compare stream_id t.max_client_stream_id) > 0) then
    (* From RFC7540§5.1.1:
     *   [...] The identifier of a newly established stream MUST be numerically
     *   greater than all streams that the initiating endpoint has opened or
     *   reserved. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else
    (* From RFC7540§6.2:
     *   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
     *   and additionally carries a header block fragment. HEADERS frames can
     *   be sent on a stream in the "idle", "reserved (local)", "open", or
     *   "half-closed (remote)" state. *)
    let reqd =
      match Streams.get_node t.streams stream_id with
      | None ->
        let reqd =
          Reqd.create
            stream_id
            ~max_frame_size:t.settings.max_frame_size
            t.writer
            t.error_handler
            (on_stream_closed t)
        in
        Streams.add
          t.streams
          ?priority
          ~initial_window_size:t.settings.initial_window_size
          reqd;
        reqd
      | Some (Stream stream) ->
        (* From RFC7540§6.9.2:
         *   Both endpoints can adjust the initial window size for new streams by
         *   including a value for SETTINGS_INITIAL_WINDOW_SIZE in the SETTINGS
         *   frame.
         *
         * Note: we already have the stream in the priority tree, and the default
         * initial window size for new streams could have changed between adding
         * the (idle) stream and opening it. *)
        stream.flow <- t.settings.initial_window_size;
        stream.streamd
    in
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
    let partial_headers =
      { Stream.parse_state =
          AB.parse
            ~initial_buffer_size
            (Hpack.Decoder.decode_headers t.hpack_decoder)
      ; end_stream = Flags.test_end_stream flags
      }
    in
    reqd.state <- Open (PartialHeaders partial_headers);
    if not end_headers then
      t.receiving_headers_for_stream <- Some stream_id;
    handle_headers_block t reqd partial_headers flags headers_block

let process_trailer_headers t reqd response_state frame_header headers_block =
  let { Frame.stream_id; flags; _ } = frame_header in
  let end_stream = Flags.test_end_stream flags in
  if not end_stream then
    (* From RFC7540§8.1:
     *   A HEADERS frame (and associated CONTINUATION frames) can only appear
     *   at the start or end of a stream. An endpoint that receives a HEADERS
     *   frame without the END_STREAM flag set after receiving a final
     *   (non-informational) status code MUST treat the corresponding request
     *   or response as malformed (Section 8.1.2.6). *)
    set_error_and_handle t reqd `Bad_request ProtocolError
  else
    let partial_headers =
      { Stream.parse_state =
          AB.parse (Hpack.Decoder.decode_headers t.hpack_decoder)
          (* obviously true at this point. *)
      ; end_stream
      }
    in
    response_state.Reqd.trailers_parser <- Some partial_headers;
    if not Flags.(test_end_header flags) then
      t.receiving_headers_for_stream <- Some stream_id;
    (* trailer headers: RFC7230§4.4 *)
    handle_trailer_headers t reqd partial_headers flags headers_block

let process_headers_frame t { Frame.frame_header; _ } ?priority headers_block =
  let { Frame.stream_id; _ } = frame_header in
  if not Stream_identifier.(is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected
     *   stream identifier MUST respond with a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else
    match priority with
    | Some { Priority.stream_dependency; _ }
      when Stream_identifier.(stream_dependency === stream_id) ->
      (* From RFC7540§5.3.1:
       *   A stream cannot depend on itself. An endpoint MUST treat this as a
       *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
      report_stream_error t stream_id Error.ProtocolError
    | _ ->
      (match Streams.find t.streams stream_id with
      | None ->
        open_stream t frame_header ?priority headers_block
      | Some reqd ->
        (match reqd.state with
        | Idle ->
          (* From RFC7540§6.2:
           *   HEADERS frames can be sent on a stream in the "idle", "reserved
           *   (local)", "open", or "half-closed (remote)" state. *)
          open_stream t frame_header ?priority headers_block
        | Open (PartialHeaders _) ->
          (* This case is unreachable because we check that partial HEADERS
           * states must be followed by CONTINUATION frames elsewhere. *)
          assert false
        (* if we're getting a HEADERS frame at this point, they must be
         * trailers, and the END_STREAM flag needs to be set. *)
        | Open (FullHeaders rs) ->
          process_trailer_headers t reqd rs frame_header headers_block
        | Open (ActiveRequest rs) ->
          process_trailer_headers t reqd rs frame_header headers_block
        | HalfClosedRemote _
        (* From RFC7540§5.1:
         *   half-closed (remote): [...] If an endpoint receives additional
         *   frames, other than WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a
         *   stream that is in this state, it MUST respond with a stream
         *   error (Section 5.4.2) of type STREAM_CLOSED. *)
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
    t -> a Streams.PriorityTreeNode.node -> int -> unit
  =
 fun t stream n ->
  let send_window_update_frame stream_id n =
    let valid_inflow = Streams.add_inflow stream n in
    assert valid_inflow;
    let frame_info = Writer.make_frame_info stream_id in
    Writer.write_window_update t.writer frame_info n
  in
  if n > 0 then (
    let max_window_size = Settings.WindowSize.max_window_size in
    let stream_id = Streams.stream_id stream in
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
  let open Streams in
  let { Frame.flags; stream_id; payload_length; _ } = frame_header in
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else (
    (* From RFC7540§6.9:
     *   A receiver that receives a flow-controlled frame MUST always account
     *   for its contribution against the connection flow-control window,
     *   unless the receiver treats this as a connection error (Section 5.4.1).
     *   This is necessary even if the frame is in error. *)
    Streams.deduct_inflow t.streams payload_length;
    match Streams.get_node t.streams stream_id with
    | Some (Stream { streamd; _ } as stream) ->
      (match streamd.Reqd.state with
      | Open (ActiveRequest ({ request_info; _ } as stream_state)) ->
        let request_body = Reqd.request_body streamd in
        request_info.request_body_bytes
        <- Int64.(
             add
               request_info.request_body_bytes
               (of_int (Bigstringaf.length bstr)));
        let request = request_info.request in
        if not Streams.(allowed_to_receive t.streams stream payload_length)
        then
          (* From RFC7540§6.9:
           *  A receiver MAY respond with a stream error (Section 5.4.2) or
           *  connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR if it
           *  is unable to accept a frame. *)
          report_stream_error t stream_id Error.FlowControlError
        else (
          Streams.deduct_inflow stream payload_length;
          match Message.unique_content_length_values request.headers with
          | [ content_length ]
            when (* we're getting more than the client declared? *)
                 Int64.compare
                   request_info.request_body_bytes
                   (Message.content_length_of_string content_length)
                 > 0 ->
            (* Give back connection-level flow-controlled bytes (we use payload
             * length to include any padding bytes that the frame might have
             * included - which were ignored at parse time). *)
            send_window_update t t.streams payload_length;
            (* From RFC7540§8.1.2.6:
             *   A request or response is also malformed if the value of a
             *   content-length header field does not equal the sum of the
             *   DATA frame payload lengths that form the body. *)
            set_error_and_handle t streamd `Bad_request ProtocolError
          | _ ->
            let end_stream = Flags.test_end_stream flags in
            (* XXX(anmonteiro): should we only give back flow control after we
             * delivered EOF to the request body? There's a potential flow
             * control issue right now where we're handing out connection-level
             * flow control tokens on the receipt of every DATA frame. This
             * might allow clients to send an unbounded number of bytes. *)
            if end_stream then
              if
                (* From RFC7540§6.1:
                 *   When set, bit 0 indicates that this frame is the last that
                 *   the endpoint will send for the identified stream. Setting
                 *   this flag causes the stream to enter one of the
                 *   "half-closed" states or the "closed" state (Section 5.1). *)
                Reqd.requires_output streamd
              then
                (* There's a potential race condition here if the request handler
                 * completes the response right after . *)
                streamd.state <- HalfClosedRemote stream_state
              else
                Reqd.finish_stream streamd Finished;
            (* From RFC7540§6.9.1:
             *   The receiver of a frame sends a WINDOW_UPDATE frame as it
             *   consumes data and frees up space in flow-control windows.
             *   Separate WINDOW_UPDATE frames are sent for the stream- and
             *   connection-level flow-control windows. *)
            send_window_update t t.streams payload_length;
            send_window_update t stream payload_length;
            let faraday = Body.unsafe_faraday request_body in
            if not (Faraday.is_closed faraday) then (
              Faraday.schedule_bigstring faraday bstr;
              if end_stream then Body.close_reader request_body))
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
      report_connection_error t Error.ProtocolError)

let process_priority_frame t { Frame.frame_header; _ } priority =
  let { Frame.stream_id; _ } = frame_header in
  let { Priority.stream_dependency; _ } = priority in
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else if Stream_identifier.(stream_id === stream_dependency) then
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a stream
     *   error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error.ProtocolError
  else
    match Streams.get_node t.streams stream_id with
    | Some stream ->
      Streams.reprioritize_stream t.streams ~priority stream
    | None ->
      (* From RFC7540§5.1.1:
       *   Endpoints SHOULD process PRIORITY frames, though they can be ignored
       *   if the stream has been removed from the dependency tree (see Section
       *   5.3.4).
       *
       * Note:
       *   if we're receiving a PRIORITY frame for a stream that we already
       *   removed from the tree (i.e. can't be found in the hash table, and
       *   for which the stream ID is strictly smaller than the maximum stream
       *   id that the client has opened), don't bother processing it. *)
      if Stream_identifier.(stream_id >= t.max_client_stream_id) then
        let reqd =
          Reqd.create
            stream_id
            ~max_frame_size:t.settings.max_frame_size
            t.writer
            t.error_handler
            (on_stream_closed t)
        in
        Streams.add
          t.streams
          ~priority
          ~initial_window_size:t.settings.initial_window_size
          reqd

let process_rst_stream_frame t { Frame.frame_header; _ } error_code =
  let { Frame.stream_id; _ } = frame_header in
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else
    match Streams.find t.streams stream_id with
    | Some reqd ->
      (match reqd.state with
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
        Reqd.finish_stream reqd (ResetByThem error_code))
    | None ->
      (* We might have removed the stream from the hash table. If its stream
       * id is strictly smaller than the max client stream id we've seen, then
       * it must have been closed. *)
      if Stream_identifier.(stream_id >= t.max_client_stream_id) then
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
  let open Streams in
  (* TODO: what to do in the case of receiving an acked SETTINGS frame for
   * settings we didn't send? *)
  let { Frame.flags; _ } = frame_header in
  (* We already checked that an acked SETTINGS is empty. Don't need to do
   * anything else in that case *)
  if not Flags.(test_ack flags) then
    match Settings.check_settings_list settings with
    | None ->
      (* From RFC7540§6.5:
       *   Each parameter in a SETTINGS frame replaces any existing value for that
       *   parameter. Parameters are processed in the order in which they appear,
       *   and a receiver of a SETTINGS frame does not need to maintain any state
       *   other than the current value of its parameters. *)
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
             *   [...] a SETTINGS frame can alter the initial flow-control window
             *   size for streams with active flow-control windows (that is,
             *   streams in the "open" or "half-closed (remote)" state). When the
             *   value of SETTINGS_INITIAL_WINDOW_SIZE changes, a receiver MUST
             *   adjust the size of all stream flow-control windows that it
             *   maintains by the difference between the new value and the old
             *   value. *)
            let old_val = t.settings.initial_window_size in
            t.settings.initial_window_size <- new_val;
            let growth = new_val - old_val in
            let exception Local in
            (match
               Streams.iter
                 ~f:(fun stream ->
                   (* From RFC7540§6.9.2:
                    *   An endpoint MUST treat a change to
                    *   SETTINGS_INITIAL_WINDOW_SIZE that causes any flow-control
                    *   window to exceed the maximum size as a connection error
                    *   (Section 5.4.1) of type FLOW_CONTROL_ERROR. *)
                   if not (Streams.add_flow stream growth) then
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
            (* XXX: We're probably not abiding entirely by this. If we get a
             * MAX_FRAME_SIZE setting we'd need to reallocate the read buffer?
             * This will need support from the I/O runtimes. *)
            t.settings.max_frame_size <- x;
            Streams.iter
              ~f:(fun (Stream { streamd; _ }) ->
                if Reqd.requires_output streamd then
                  streamd.max_frame_size <- x)
              t.streams
          | MaxHeaderListSize, x ->
            t.settings.max_header_list_size <- Some x)
        settings;
      (* From RFC7540§6.5.2:
       *   Once all values have been processed, the recipient MUST immediately
       *   emit a SETTINGS frame with the ACK flag set. *)
      let frame_info =
        Writer.make_frame_info
          ~flags:Flags.(set_ack default_flags)
          Stream_identifier.connection
      in
      (* From RFC7540§6.5:
       *   ACK (0x1): [...] When this bit is set, the payload of the SETTINGS
       *   frame MUST be empty. *)
      Writer.write_settings t.writer frame_info [];
      wakeup_writer t
    | Some error ->
      handle_error t error

let process_ping_frame t { Frame.frame_header; _ } payload =
  let { Frame.flags; _ } = frame_header in
  (* From RFC7540§6.7:
   *   ACK (0x1): When set, bit 0 indicates that this PING frame is a PING
   *   response. [...] An endpoint MUST NOT respond to PING frames containing
   *   this flag. *)
  if not (Flags.test_ack flags) then (
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
    wakeup_writer t)

let process_goaway_frame t _frame payload =
  let _last_stream_id, _error, debug_data = payload in
  let len = Bigstringaf.length debug_data in
  let bytes = Bytes.create len in
  Bigstringaf.unsafe_blit_to_bytes debug_data ~src_off:0 bytes ~dst_off:0 ~len;
  (* TODO(anmonteiro): I think we need to allow lower numbered streams to
   * complete. *)
  shutdown t

let add_window_increment : type a.
    t -> a Streams.PriorityTreeNode.node -> int -> unit
  =
 fun t stream increment ->
  let open Streams in
  let did_add = Streams.add_flow stream increment in
  let stream_id = Streams.stream_id stream in
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
  let open Streams in
  let { Frame.stream_id; _ } = frame_header in
  (* From RFC7540§6.9:
   *   The WINDOW_UPDATE frame can be specific to a stream or to the entire
   *   connection. In the former case, the frame's stream identifier indicates
   *   the affected stream; in the latter, the value "0" indicates that the
   *   entire connection is the subject of the frame. *)
  if Stream_identifier.is_connection stream_id then
    add_window_increment t t.streams window_increment
  else
    match Streams.get_node t.streams stream_id with
    | Some (Stream { streamd; _ } as stream_node) ->
      (match streamd.state with
      | Idle ->
        (* From RFC7540§5.1:
         *   idle: [...] Receiving any frame other than HEADERS or PRIORITY on
         *   a stream in this state MUST be treated as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
        report_connection_error t Error.ProtocolError
      | Open _
      (* From RFC7540§6.9:
       *   [...] a receiver could receive a WINDOW_UPDATE frame on a
       *   "half-closed (remote)" or "closed" stream. A receiver MUST NOT treat
       *   this as an error (see Section 5.1). *)
      | HalfClosedRemote _
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
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error.ProtocolError
  else
    match Streams.find t.streams stream_id with
    | Some stream ->
      (match stream.Reqd.state with
      | Open (PartialHeaders partial_headers) ->
        handle_headers_block t stream partial_headers flags headers_block
      | Open (ActiveRequest { trailers_parser = Some partial_headers; _ }) ->
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

let default_error_handler ?request:_ error handle =
  let message =
    match error with
    | `Exn exn ->
      Printexc.to_string exn
    | (#Status.client_error | #Status.server_error) as error ->
      Status.to_string error
  in
  let body = handle Headers.empty in
  Body.write_string body message;
  Body.close_writer body

let create
    ?(config = Config.default)
    ?(error_handler = default_error_handler)
    request_handler
  =
  let { Config.response_buffer_size; _ } = config in
  let settings =
    { Settings.default_settings with
      max_frame_size = config.read_buffer_size
    ; max_concurrent_streams = config.max_concurrent_streams
    ; initial_window_size = config.initial_window_size
    ; enable_push = config.enable_server_push
    }
  in
  let writer = Writer.create response_buffer_size in
  let rec connection_preface_handler recv_frame settings_list =
    let t = Lazy.force t in
    t.reader <- Active (Reader.frame (frame_handler t));
    (* Check if the settings for the connection are different than the default
     * HTTP/2 settings. In the event that they are, we need to send a non-empty
     * SETTINGS frame advertising our configuration. *)
    let settings = Settings.settings_for_the_connection settings in
    (* This is the connection preface. We don't set the ack flag. *)
    let frame_info = Writer.make_frame_info Stream_identifier.connection in
    (* This is our SETTINGS frame. *)
    (* From RFC7540§3.5:
     *   The server connection preface consists of a potentially empty
     *   SETTINGS frame (Section 6.5) that MUST be the first frame the
     *   server sends in the HTTP/2 connection. *)
    Writer.write_settings t.writer frame_info settings;
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
        | PushPromise _ ->
          (* From RFC7540§8.2:
           *   A client cannot push. Thus, servers MUST treat the receipt of a
           *   PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
           *   PROTOCOL_ERROR. *)
          report_connection_error
            t
            ~additional_debug_data:"Client cannot push"
            Error.ProtocolError
        | Ping data ->
          process_ping_frame t frame data
        | GoAway (last_stream_id, error, debug_data) ->
          process_goaway_frame t frame (last_stream_id, error, debug_data)
        | WindowUpdate window_size ->
          process_window_update_frame t frame window_size
        | Continuation headers_block ->
          process_continuation_frame t frame headers_block
        | Unknown _ ->
          (* TODO: in the future we can expose a hook for handling unknown
           * frames, e.g. the ALTSVC frame defined in RFC7838§4
           * (https://tools.ietf.org/html/rfc7838#section-4) *)
          (* From RFC7540§5.1:
           *   Frames of unknown types are ignored. *)
          ()))
  and t =
    lazy
      { settings
      ; reader =
          New (Reader.client_connection_preface connection_preface_handler)
      ; writer
      ; config
      ; request_handler
      ; error_handler
      ; streams = Streams.make_root ()
      ; current_client_streams = 0
      ; max_client_stream_id = 0l
      ; max_pushed_stream_id = 0l
      ; receiving_headers_for_stream = None
      ; did_send_go_away = false
      ; wakeup_writer = ref default_wakeup_writer
      ; hpack_encoder = Hpack.Encoder.(create settings.header_table_size)
      ; hpack_decoder = Hpack.Decoder.(create settings.header_table_size)
      }
  in
  Lazy.force t

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

let flush_response_body t =
  if is_active t then
    Streams.flush t.streams

let next_write_operation t =
  flush_response_body t;
  Writer.next t.writer

let report_write_result t result = Writer.report_result t.writer result

let yield_writer t k =
  if is_active t then (
    let { max_client_stream_id; max_pushed_stream_id; _ } = t in
    Streams.on_more_output_available
      t.streams
      (max_client_stream_id, max_pushed_stream_id)
      k;
    (* We need to let both the connection and each individual streams wake up
     * the connection, given that some control frames can be scheduled to be
     * written when all the streams are yielding. *)
    on_wakeup_writer t k)
  else if Writer.is_closed t.writer then
    k ()
  else
    on_wakeup_writer t k
