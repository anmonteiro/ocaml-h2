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

  type t = Reqd.t

  let flush_write_body = Reqd.flush_response_body

  let requires_output = Reqd.requires_output
end)

type request_handler = Reqd.t -> unit

type error =
  [ `Bad_request
  | `Internal_server_error
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [ `write ] Body.t) -> unit

type t =
  { mutable settings : Settings.t
  ; reader : Reader.frame
  ; writer : Writer.t
  ; config : Config.t
  ; request_handler : request_handler
  ; error_handler : error_handler
  ; streams : Scheduler.t
        (* Number of currently open client streams. Used for
         * MAX_CONCURRENT_STREAMS bookkeeping *)
  ; mutable current_client_streams : int
  ; mutable max_client_stream_id : Stream_identifier.t
  ; mutable max_pushed_stream_id : Stream_identifier.t
  ; mutable receiving_headers_for_stream : Stream_identifier.t option
        (* Keep track of number of SETTINGS frames that we sent and for which
         * we haven't eceived an acknowledgment from the client. *)
  ; mutable unacked_settings : int
  ; mutable did_send_go_away : bool
        (* From RFC7540§4.3:
         *   Header compression is stateful. One compression context and one
         *   decompression context are used for the entire connection. *)
  ; hpack_encoder : Hpack.Encoder.t
  ; hpack_decoder : Hpack.Decoder.t
  }

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer

let wakeup_writer t = Writer.wakeup t.writer

let shutdown_reader t = Reader.force_close t.reader

let shutdown_writer t =
  Writer.close t.writer;
  wakeup_writer t

let shutdown t =
  shutdown_reader t;
  shutdown_writer t

(* Handling frames against closed streams is hard. See:
 * https://docs.google.com/presentation/d/1iG_U2bKTc9CnKr0jPTrNfmxyLufx_cK2nNh9VjrKH6s
 *)
let was_closed_or_implicitly_closed t stream_id =
  if Stream_identifier.is_request stream_id then
    Stream_identifier.(stream_id <= t.max_client_stream_id)
  else
    Stream_identifier.(stream_id <= t.max_pushed_stream_id)

(* TODO: currently connection-level errors are not reported to the error
 * handler because it is assumed that an error handler will produce a response,
 * and since HTTP/2 is multiplexed, there's no matching response for a
 * connection error. We should do something about it. *)
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
        ~last_stream_id:t.max_client_stream_id
        error;
      Writer.flush t.writer (fun () ->
          (* XXX: We need to allow lower numbered streams to complete before
           * shutting down. *)
          shutdown t);
      t.did_send_go_away <- true;
      wakeup_writer t)
  | StreamError (stream_id, error) ->
    (match Scheduler.find t.streams stream_id with
    | Some reqd ->
      Stream.reset_stream reqd error
    | None ->
      if not (was_closed_or_implicitly_closed t stream_id) then
        (* Possible if the stream was going to enter the Idle state (first time
         * we saw e.g. a PRIORITY frame for it) but had e.g. a
         * FRAME_SIZE_ERROR. *)
        let frame_info = Writer.make_frame_info stream_id in
        Writer.write_rst_stream t.writer frame_info error);
    wakeup_writer t

let report_connection_error t ?(additional_debug_data = "") error =
  report_error t (ConnectionError (error, additional_debug_data))

let report_stream_error t stream_id error =
  report_error t (StreamError (stream_id, error))

let set_error_and_handle ?request t stream error error_code =
  assert (request = None);
  Reqd.report_error stream error error_code;
  wakeup_writer t

let report_exn t exn =
  if not (is_closed t) then
    let additional_debug_data = Printexc.to_string exn in
    report_connection_error t ~additional_debug_data Error_code.InternalError

let on_close_stream t id ~active closed =
  if active then
    (* From RFC7540§5.1.2:
     *   Streams that are in the "open" state or in either of the "half-closed"
     *   states count toward the maximum number of streams that an endpoint is
     *   permitted to open. *)
    t.current_client_streams <- t.current_client_streams - 1;
  Scheduler.mark_for_removal t.streams id closed

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
    wakeup_writer t)

let create_push_stream t parent_stream_id =
  let candidate_push_stream_id = Int32.add t.max_pushed_stream_id 2l in
  if not t.settings.enable_push then
    (* From RFC7540§6.6:
     *   PUSH_PROMISE MUST NOT be sent if the SETTINGS_ENABLE_PUSH setting of
     *   the peer endpoint is set to 0. *)
    Error `Push_disabled
  else if Stream_identifier.(candidate_push_stream_id > max_stream_id) then (
    (* From RFC7540§5.1:
     *   Stream identifiers cannot be reused. Long-lived connections can result
     *   in an endpoint exhausting the available range of stream identifiers.
     *   [...] A server that is unable to establish a new stream identifier can
     *   send a GOAWAY frame so that the client is forced to open a new
     *   connection for new streams. *)
    report_connection_error t Error_code.NoError;
    Error `Stream_ids_exhausted)
  else
    let pushed_stream_id = candidate_push_stream_id in
    t.max_pushed_stream_id <- pushed_stream_id;
    let reqd =
      Stream.create
        pushed_stream_id
        ~max_frame_size:t.settings.max_frame_size
        t.writer
        t.error_handler
        (on_close_stream t pushed_stream_id)
    in
    (* From RFC7540§5.3.5:
     *   All streams are initially assigned a non-exclusive dependency on
     *   stream 0x0. Pushed streams (Section 8.2) initially depend on their
     *   associated stream. In both cases, streams are assigned a default
     *   weight of 16. *)
    let _stream : Scheduler.nonroot Scheduler.node =
      Scheduler.add
        t.streams
        ~priority:
          { Priority.default_priority with
            stream_dependency = parent_stream_id
          }
        ~initial_send_window_size:t.settings.initial_window_size
        ~initial_recv_window_size:t.config.initial_window_size
        reqd
    in
    Ok reqd

let handle_headers t ~end_stream stream active_stream headers =
  let (Scheduler.Stream { descriptor = reqd; _ }) = stream in
  (* From RFC7540§5.1.2:
   *   Endpoints MUST NOT exceed the limit set by their peer. An endpoint that
   *   receives a HEADERS frame that causes its advertised concurrent stream
   *   limit to be exceeded MUST treat this as a stream error (Section 5.4.2)
   *   of type PROTOCOL_ERROR or REFUSED_STREAM. *)
  if
    Int32.(
      compare
        (of_int (t.current_client_streams + 1))
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
      report_stream_error t reqd.Stream.id Error_code.RefusedStream
    else
      report_stream_error t reqd.Stream.id Error_code.ProtocolError
  else (
    reqd.state <- Active (Open FullHeaders, active_stream);
    (* From RFC7540§5.1.2:
     *   Streams that are in the "open" state or in either of the "half-closed"
     *   states count toward the maximum number of streams that an endpoint is
     *   permitted to open. *)
    t.current_client_streams <- t.current_client_streams + 1;
    match Headers.method_path_and_scheme_or_malformed headers with
    | `Malformed ->
      (* From RFC7540§8.1.2.6:
       *   For malformed requests, a server MAY send an HTTP response prior to
       *   closing or resetting the stream. *)
      set_error_and_handle t reqd `Bad_request ProtocolError
    | `Valid (meth, path, scheme) ->
      (* Note: we don't need to check for `end_stream` flag + a non-zero body
       * length, as the spec allows for non-zero content-length headers and no
       * DATA frames.
       *
       * From RFC7540§8.1.2.6:
       *   A response that is defined to have no payload, as described in
       *   [RFC7230], Section 3.3.2, can have a non-zero content-length header
       *   field, even though no content is included in DATA frames. *)
      (match Message.body_length headers with
      | `Error e ->
        set_error_and_handle t reqd e ProtocolError
      | `Fixed _ | `Unknown ->
        let request =
          Request.create ~scheme ~headers (Httpaf.Method.of_string meth) path
        in
        let request_body =
          if end_stream then
            Body.empty
          else
            Body.create_reader
              (Bigstringaf.create t.config.request_body_buffer_size)
              ~done_reading:(fun len ->
                let len = Int32.of_int len in
                (* From RFC7540§6.9.1:
                 *   The receiver of a frame sends a WINDOW_UPDATE frame as it
                 *   consumes data and frees up space in flow-control windows.
                 *   Separate WINDOW_UPDATE frames are sent for the stream- and
                 *   connection-level flow-control windows. *)
                match reqd.state with
                | Active _ ->
                  send_window_update t t.streams len;
                  send_window_update t stream len
                | Idle | Reserved _ | Closed _ ->
                  ())
        in
        let request_info = Reqd.create_active_request request request_body in
        if end_stream then (
          (* From RFC7540§5.1:
           *   [...] an endpoint receiving an END_STREAM flag causes the stream
           *   state to become "half-closed (remote)". *)
          reqd.state <- Active (HalfClosed request_info, active_stream);
          (* Deliver EOF to the request body, as the handler might be waiting
           * on it to produce a response. *)
          Body.close_reader request_body)
        else
          reqd.state <- Active (Open (ActiveMessage request_info), active_stream);
        t.request_handler reqd;
        wakeup_writer t))

let handle_headers_block
    t
    ?(is_trailers = false)
    stream
    active_stream
    partial_headers
    flags
    headers_block
  =
  let open AB in
  let (Scheduler.Stream { descriptor = reqd; _ }) = stream in
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
        t.max_client_stream_id <- reqd.Stream.id;
        (* `handle_headers` will take care of transitioning the stream state *)
        handle_headers
          t
          ~end_stream:partial_headers.end_stream
          stream
          active_stream
          headers)
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
      report_connection_error t Error_code.CompressionError
    | Fail (_, _, message) ->
      report_connection_error
        t
        ~additional_debug_data:message
        Error_code.CompressionError)
  else
    partial_headers.parse_state <- parse_state'

let handle_trailer_headers = handle_headers_block ~is_trailers:true

let open_stream t ~priority stream_id =
  if not Stream_identifier.(stream_id > t.max_client_stream_id) then (
    (* From RFC7540§5.1.1:
     *   [...] The identifier of a newly established stream MUST be numerically
     *   greater than all streams that the initiating endpoint has opened or
     *   reserved. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError;
    None)
  else
    (* From RFC7540§6.2:
     *   The HEADERS frame (type=0x1) is used to open a stream (Section 5.1),
     *   and additionally carries a header block fragment. HEADERS frames can
     *   be sent on a stream in the "idle", "reserved (local)", "open", or
     *   "half-closed (remote)" state. *)
    match Scheduler.get_node t.streams stream_id with
    | None ->
      let reqd =
        Stream.create
          stream_id
          ~max_frame_size:t.settings.max_frame_size
          t.writer
          t.error_handler
          (on_close_stream t stream_id)
      in
      let stream : Scheduler.nonroot Scheduler.node =
        Scheduler.add
          t.streams
          ~priority
          ~initial_send_window_size:t.settings.initial_window_size
          ~initial_recv_window_size:t.config.initial_window_size
          reqd
      in
      Some stream
    | Some (Scheduler.Stream node as stream) ->
      (* From RFC7540§6.9.2:
       *   Both endpoints can adjust the initial window size for new streams
       *   by including a value for SETTINGS_INITIAL_WINDOW_SIZE in the
       *   SETTINGS frame.
       *
       * Note: we already have the stream in the priority tree, and the
       * default initial window size for new streams could have changed
       * between adding the (idle) stream and opening it.
       *
       * Note: `inflow` doesn't change, that's set by us statically via config.
       *)
      node.flow <- t.settings.initial_window_size;
      Some stream

let process_first_headers_block t frame_header stream headers_block =
  let (Scheduler.Stream { descriptor = reqd; _ }) = stream in
  let { Frame.stream_id; flags; _ } = frame_header in
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
  let active_stream =
    Reqd.create_active_stream
      t.hpack_encoder
      t.config.response_body_buffer_size
      (create_push_stream t)
  in
  reqd.Stream.state <-
    Active (Open (PartialHeaders partial_headers), active_stream);
  if not end_headers then
    t.receiving_headers_for_stream <- Some stream_id;
  handle_headers_block
    t
    stream
    active_stream
    partial_headers
    flags
    headers_block

let process_trailer_headers t stream active_stream frame_header headers_block =
  let (Scheduler.Stream { descriptor = reqd; _ }) = stream in
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
    active_stream.Reqd.trailers_parser <- Some partial_headers;
    if not Flags.(test_end_header flags) then
      t.receiving_headers_for_stream <- Some stream_id;
    (* trailer headers: RFC7230§4.4 *)
    handle_trailer_headers
      t
      stream
      active_stream
      partial_headers
      flags
      headers_block

let process_headers_frame t { Frame.frame_header; _ } ~priority headers_block =
  let { Frame.stream_id; _ } = frame_header in
  let { Priority.stream_dependency; _ } = priority in
  if not Stream_identifier.(is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected
     *   stream identifier MUST respond with a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError
  else if Stream_identifier.(stream_dependency === stream_id) then
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a
     *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error_code.ProtocolError
  else
    match Scheduler.get_node t.streams stream_id with
    | None ->
      (match open_stream t ~priority stream_id with
      | Some reqd ->
        process_first_headers_block t frame_header reqd headers_block
      | None ->
        ())
    | Some (Scheduler.Stream { descriptor = reqd; _ } as stream) ->
      (match reqd.state with
      | Idle ->
        (* From RFC7540§6.2:
         *   HEADERS frames can be sent on a stream in the "idle", "reserved
         *   (local)", "open", or "half-closed (remote)" state. *)
        (match open_stream t ~priority stream_id with
        | Some reqd ->
          process_first_headers_block t frame_header reqd headers_block
        | None ->
          ())
      | Active (Open (WaitingForPeer | PartialHeaders _), _) ->
        (* This case is unreachable because we check that partial HEADERS
         * states must be followed by CONTINUATION frames elsewhere. *)
        assert false
      (* if we're getting a HEADERS frame at this point, they must be
       * trailers, and the END_STREAM flag needs to be set. *)
      | Active (Open (FullHeaders | ActiveMessage _), active_stream) ->
        process_trailer_headers
          t
          stream
          active_stream
          frame_header
          headers_block
      | Active (HalfClosed _, _)
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
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError
  else
    (* From RFC7540§6.9:
     *   A receiver that receives a flow-controlled frame MUST always account
     *   for its contribution against the connection flow-control window,
     *   unless the receiver treats this as a connection error (Section 5.4.1).
     *   This is necessary even if the frame is in error. *)
    let payload_len32 = Int32.of_int payload_length in
    Scheduler.deduct_inflow t.streams payload_len32;
    match Scheduler.get_node t.streams stream_id with
    | Some (Stream { descriptor; _ } as stream) ->
      (match descriptor.state with
      | Active (Open (ActiveMessage request_info), active_stream) ->
        let request_body = Reqd.request_body descriptor in
        request_info.request_body_bytes <-
          Int64.(
            add
              request_info.request_body_bytes
              (of_int (Bigstringaf.length bstr)));
        let request = request_info.request in
        if not Scheduler.(allowed_to_receive t.streams stream payload_len32)
        then (
          (* From RFC7540§6.9:
           *  A receiver MAY respond with a stream error (Section 5.4.2) or
           *  connection error (Section 5.4.1) of type FLOW_CONTROL_ERROR if it
           *  is unable to accept a frame. *)
          send_window_update t t.streams payload_len32;
          report_stream_error t stream_id Error_code.FlowControlError)
        else (
          Scheduler.deduct_inflow stream payload_len32;
          match Message.body_length request.headers with
          | `Fixed len
          (* Getting more than the client declared *)
            when Int64.compare request_info.request_body_bytes len > 0 ->
            (* Give back connection-level flow-controlled bytes (we use payload
             * length to include any padding bytes that the frame might have
             * included - which were ignored at parse time). *)
            send_window_update t t.streams payload_len32;
            (* From RFC7540§8.1.2.6:
             *   A request or response is also malformed if the value of a
             *   content-length header field does not equal the sum of the
             *   DATA frame payload lengths that form the body. *)
            set_error_and_handle t descriptor `Bad_request ProtocolError
          | _ ->
            let end_stream = Flags.test_end_stream flags in
            if end_stream then
              if
                (* From RFC7540§6.1:
                 *   When set, bit 0 indicates that this frame is the last that
                 *   the endpoint will send for the identified stream. Setting
                 *   this flag causes the stream to enter one of the
                 *   "half-closed" states or the "closed" state
                 *   (Section 5.1). *)
                Reqd.requires_output descriptor
              then
                (* There's a potential race condition here if the request
                 * handler completes the response right after. *)
                descriptor.state <-
                  Active (HalfClosed request_info, active_stream)
              else
                Stream.finish_stream descriptor Finished;
            (* From RFC7540§6.9.1:
             *   The receiver of a frame sends a WINDOW_UPDATE frame as it
             *   consumes data and frees up space in flow-control windows.
             *   Separate WINDOW_UPDATE frames are sent for the stream- and
             *   connection-level flow-control windows.
             *
             * Note: we send these WINDOW_UPDATE frames once the body bytes
             * have been surfaced to the application. This is done in the
             * record field `done_reading` of `Body.t`. *)
            let faraday = Body.unsafe_faraday request_body in
            if not (Faraday.is_closed faraday) then (
              Faraday.schedule_bigstring faraday bstr;
              if end_stream then Body.close_reader request_body);
            Reqd.flush_request_body descriptor)
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

let process_priority_frame t { Frame.frame_header; _ } priority =
  let { Frame.stream_id; _ } = frame_header in
  let { Priority.stream_dependency; _ } = priority in
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError
  else if Stream_identifier.(stream_id === stream_dependency) then
    (* From RFC7540§5.3.1:
     *   A stream cannot depend on itself. An endpoint MUST treat this as a
     *   stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error_code.ProtocolError
  else
    match Scheduler.get_node t.streams stream_id with
    | Some stream ->
      Scheduler.reprioritize_stream t.streams ~priority stream
    | None ->
      (* From RFC7540§5.1.1:
       *   Endpoints SHOULD process PRIORITY frames, though they can be ignored
       *   if the stream has been removed from the dependency tree (see Section
       *   5.3.4).
       *
       * Note:
       *   if we're receiving a PRIORITY frame for a stream that we already
       *   removed from the tree (i.e. can't be found in the hash table, and
       *   for which the stream ID is smaller then or equal to the max stream
       *   id that the client has opened), don't bother processing it. *)
      if not (was_closed_or_implicitly_closed t stream_id) then
        let reqd =
          Stream.create
            stream_id
            ~max_frame_size:t.settings.max_frame_size
            t.writer
            t.error_handler
            (on_close_stream t stream_id)
        in
        let _stream : Scheduler.nonroot Scheduler.node =
          Scheduler.add
            t.streams
            ~priority
            ~initial_send_window_size:t.settings.initial_window_size
            ~initial_recv_window_size:t.config.initial_window_size
            reqd
        in
        ()

let process_rst_stream_frame t { Frame.frame_header; _ } error_code =
  let { Frame.stream_id; _ } = frame_header in
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError
  else
    match Scheduler.find t.streams stream_id with
    | Some reqd ->
      (match reqd.state with
      | Idle ->
        (* From RFC7540§6.4:
         *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
         *   state. If a RST_STREAM frame identifying an idle stream is
         *   received, the recipient MUST treat this as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
        report_connection_error t Error_code.ProtocolError
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
        Stream.finish_stream reqd (ResetByThem error_code))
    | None ->
      (* We might have removed the stream from the hash table. If its stream
       * id is smaller than or equal to the max client stream id we've seen,
       * then it must have been closed. *)
      if not (was_closed_or_implicitly_closed t stream_id) then
        (* From RFC7540§6.4:
         *   RST_STREAM frames MUST NOT be sent for a stream in the "idle"
         *   state. If a RST_STREAM frame identifying an idle stream is
         *   received, the recipient MUST treat this as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR.
         *
         * Note:
         *   If we didn't find the stream in the hash table it must be
         *   "idle". *)
        report_connection_error t Error_code.ProtocolError

let apply_settings_list t settings =
  let open Scheduler in
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
           *   [...] a SETTINGS frame can alter the initial flow-control
           *   window size for streams with active flow-control windows (that
           *   is, streams in the "open" or "half-closed (remote)" state).
           *   When the value of SETTINGS_INITIAL_WINDOW_SIZE changes, a
           *   receiver MUST adjust the size of all stream flow-control
           *   windows that it maintains by the difference between the new
           *   value and the old value. *)
          let old_val = acc.initial_window_size in
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
          (* XXX: We're probably not abiding entirely by this. If we get a
           * MAX_FRAME_SIZE setting we'd need to reallocate the read buffer?
           * This will need support from the I/O runtimes. *)
          Scheduler.iter
            ~f:(fun (Stream { descriptor; _ }) ->
              if Reqd.requires_output descriptor then
                descriptor.max_frame_size <- x)
            t.streams;
          { acc with max_frame_size = x }
        | MaxHeaderListSize x ->
          { acc with max_header_list_size = Some x })
      t.settings
      settings
  in
  t.settings <- new_settings

let write_settings_frame t ~ack settings =
  let flags =
    if ack then Flags.(set_ack default_flags) else Flags.default_flags
  in
  let frame_info = Writer.make_frame_info ~flags Stream_identifier.connection in
  Writer.write_settings t.writer frame_info settings;
  if not ack then
    (* Don't expected our acknowledgements to be acknowledged... *)
    t.unacked_settings <- t.unacked_settings + 1

let process_settings_frame t { Frame.frame_header; _ } settings =
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
    match Settings.check_settings_list settings with
    | Ok () ->
      apply_settings_list t settings;
      (* From RFC7540§6.5.2:
       *   Once all values have been processed, the recipient MUST immediately
       *   emit a SETTINGS frame with the ACK flag set.
       *
       * From RFC7540§6.5:
       *   ACK (0x1): [...] When this bit is set, the payload of the SETTINGS
       *   frame MUST be empty. *)
      write_settings_frame t ~ack:true [];
      wakeup_writer t
    | Error error ->
      report_error t error

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
      wakeup_writer t)
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
        (* From RFC7540§6.9:
         *   [...] a receiver could receive a WINDOW_UPDATE frame on a
         *   "half-closed (remote)" or "closed" stream. A receiver MUST NOT
         *   treat this as an error (see Section 5.1). *)
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
  if not (Stream_identifier.is_request stream_id) then
    (* From RFC7540§5.1.1:
     *   Streams initiated by a client MUST use odd-numbered stream
     *   identifiers. [...] An endpoint that receives an unexpected stream
     *   identifier MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    report_connection_error t Error_code.ProtocolError
  else
    match Scheduler.get_node t.streams stream_id with
    | Some (Scheduler.Stream { descriptor; _ } as stream) ->
      (match descriptor.state with
      | Active (Open (PartialHeaders partial_headers), active_stream) ->
        handle_headers_block
          t
          stream
          active_stream
          partial_headers
          flags
          headers_block
      | Active
          ( Open (ActiveMessage _)
          , ({ trailers_parser = Some partial_headers; _ } as active_stream) )
        ->
        handle_trailer_headers
          t
          stream
          active_stream
          partial_headers
          flags
          headers_block
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
       *   CONTINUATION frame without the END_HEADERS flag set. A recipient
       *   that observes violation of this rule MUST respond with a connection
       *   error (Section 5.4.1) of type PROTOCOL_ERROR. *)
      report_connection_error t Error_code.ProtocolError

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

let write_connection_preface t =
  (* Check if the settings for the connection are different than the default
   * HTTP/2 settings. In the event that they are, we need to send a non-empty
   * SETTINGS frame advertising our configuration. *)
  let settings = Settings.settings_for_the_connection t.settings in
  (* XXX(anmonteiro): same as in the client. Revert
   * [t.settings.initial_window_size] to the spec-default value until we
   * receive a setting for it. *)
  t.settings <-
    { t.settings with
      initial_window_size = Settings.default.initial_window_size
    };
  (* Now send the connection preface, including our settings for the
   * connection.
   *
   * From RFC7540§3.5:
   *   The server connection preface consists of a potentially empty
   *   SETTINGS frame (Section 6.5) that MUST be the first frame the
   *   server sends in the HTTP/2 connection. *)
  write_settings_frame ~ack:false t settings;
  (* If a higher value for initial window size is configured, add more
   * tokens to the connection (we have no streams at this point). *)
  if t.config.initial_window_size > Settings.default.initial_window_size then
    let diff =
      Int32.sub
        t.config.initial_window_size
        Settings.default.initial_window_size
    in
    send_window_update t t.streams diff

let create_generic ~h2c ~config ~error_handler request_handler =
  let settings = Config.to_settings config in
  let writer = Writer.create settings.max_frame_size in
  let rec connection_preface_handler recv_frame settings_list =
    let t = Lazy.force t in
    (* If this connection is `h2c` (HTTP/2 over TCP), we have already written
     * the server connection preface. This is only necessary if we're
     * responding to a client-initiated connection, but in `h2c` the server
     * writes the preface first (handled below in `create_h2c`) *)
    if not h2c then
      write_connection_preface t;
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
        | Headers (priority, headers_block) ->
          process_headers_frame t frame ~priority headers_block
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
            Error_code.ProtocolError
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
          Reader.server_frames
            ~max_frame_size:settings.max_frame_size
            connection_preface_handler
            frame_handler
      ; writer
      ; config
      ; request_handler
      ; error_handler
      ; streams = Scheduler.make_root ()
      ; current_client_streams = 0
      ; max_client_stream_id = 0l
      ; max_pushed_stream_id = 0l
      ; receiving_headers_for_stream = None
      ; did_send_go_away = false
      ; unacked_settings = 0
      ; hpack_encoder = Hpack.Encoder.(create settings.header_table_size)
      ; hpack_decoder = Hpack.Decoder.(create settings.header_table_size)
      }
  in
  Lazy.force t

let create
    ?(config = Config.default)
    ?(error_handler = default_error_handler)
    request_handler
  =
  (* `h2c` false = direct *)
  create_generic ~h2c:false ~config ~error_handler request_handler

let handle_h2c_request t headers request_body_iovecs =
  (* From RFC7540§3.2:
   *   The HTTP/1.1 request that is sent prior to upgrade is assigned a stream
   *   identifier of 1 (see Section 5.1.1) with default priority values
   *   (Section 5.3.5). *)
  match open_stream ~priority:Priority.default_priority t 1l with
  | Some (Scheduler.Stream { descriptor = reqd; _ } as stream) ->
    let active_stream =
      Reqd.create_active_stream
        t.hpack_encoder
        t.config.response_body_buffer_size
        (create_push_stream t)
    in
    t.max_client_stream_id <- reqd.Stream.id;
    let lengthv = Httpaf.IOVec.lengthv request_body_iovecs in
    let end_stream = lengthv = 0 in
    handle_headers t ~end_stream stream active_stream headers;
    let request = Reqd.request reqd in
    let request_body = Reqd.request_body reqd in
    let request_info = Reqd.create_active_request request request_body in
    request_info.request_body_bytes <-
      Int64.(add request_info.request_body_bytes (of_int lengthv));
    (* From RFC7540§3.2:
     *   Stream 1 is implicitly "half-closed" from the client toward the server
     *   (see Section 5.1), since the request is completed as an HTTP/1.1
     *   request. After commencing the HTTP/2 connection, stream 1 is used for
     *   the response. *)
    reqd.state <- Active (HalfClosed request_info, active_stream);
    if not end_stream then
      let faraday = Body.unsafe_faraday request_body in
      if not (Faraday.is_closed faraday) then (
        List.iter
          (fun { Httpaf.IOVec.buffer; off; len } ->
            Faraday.schedule_bigstring faraday ~off ~len buffer)
          request_body_iovecs;
        (* Close the request body, we're not expecting more input. *)
        Body.close_reader request_body)
  | None ->
    ()

(* This function is meant to be called inside an HTTP/1.1 upgrade handler.
 *
 * It's useful to have the request body on the server because a single request
 * handler may process both upgraded (h2c) and direct connections. Given the
 * following, application code that calls this function needs to buffer the
 * entire request body in memory if it wants, so we can just get a
 * `Bigstringaf.t IOVec.t list`
 *
 *
 * From RFC7540§3.2:
 *   Requests that contain a payload body MUST be sent in their
 *   entirety before the client can send HTTP/2 frames. This means
 *   that a large request can block the use of the connection until
 *   it is completely sent. *)
let create_h2c
    ?(config = Config.default)
    ?(error_handler = default_error_handler)
    ~http_request
    ?(request_body = [])
    request_handler
  =
  let { Httpaf.Request.headers; _ } = http_request in
  (* From RFC7540§3.2.1:
   *   A request that upgrades from HTTP/1.1 to HTTP/2 MUST include exactly one
   *   HTTP2-Settings header field.
   *
   *   [...] A server MUST NOT upgrade the connection to HTTP/2 if this header
   *   field is not present or if more than one is present. A server MUST NOT
   *   send this header field. *)
  match
    Httpaf.Headers.(
      get_multi headers "http2-settings", get_multi headers "connection")
  with
  | [ settings ], [ connection ] when Headers.is_valid_h2c_connection connection
    ->
    (* From RFC7540§3.2.1:
     *   The content of the HTTP2-Settings header field is the payload of a
     *   SETTINGS frame (Section 6.5), encoded as a base64url string (that is,
     *   the URL- and filename-safe Base64 encoding described in Section 5 of
     *   [RFC4648], with any trailing '=' characters omitted).
     *
     *   [...] A server decodes and interprets these values as it would any
     *   other SETTINGS frame. Explicit acknowledgement of these settings
     *   (Section 6.5.3) is not necessary, since a 101 response serves as
     *   implicit acknowledgement. *)
    (match Headers.of_http1 http_request with
    | Ok h2_headers ->
      (match Settings.of_base64 settings with
      | Ok upgrade_settings ->
        let settings_list =
          Settings.settings_for_the_connection upgrade_settings
        in
        (match Settings.check_settings_list settings_list with
        | Ok () ->
          let t =
            create_generic ~h2c:true ~config ~error_handler request_handler
          in
          apply_settings_list t settings_list;
          (* From RFC7540§3.5:
           *   The first HTTP/2 frame sent by the server MUST be a server
           *   connection preface (Section 3.5) consisting of a SETTINGS
           *   frame (Section 6.5).
           *
           *   Note: as opposed to a connection started by a client, in h2c
           *   we're upgrading from HTTP/1.1 so the server is reponsible for
           *   writing the HTTP/2 connection preface to the wire first. We
           *   also configure the connection with `~h2c:true` above to not
           *   send it a second time. *)
          write_connection_preface t;
          (* From RFC7540§3.2:
           *   A server that supports HTTP/2 accepts the upgrade with a 101
           *   (Switching Protocols) response. After the empty line that
           *   terminates the 101 response, the server can begin sending
           *   HTTP/2 frames. These frames MUST include a response to the
           *   request that initiated the upgrade *)
          handle_h2c_request t h2_headers request_body;
          Ok t
        | Error error ->
          Error (Error.message error))
      | Error msg ->
        Error msg)
    | Error msg ->
      Error msg)
  | _ ->
    Error
      "A request that upgrades from HTTP/1.1 to HTTP/2 MUST include exactly \
       one HTTP2-Settings header field and HTTP2-Settings as a connection \
       option in the Connection header field."

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
  Scheduler.flush t.streams (t.max_client_stream_id, t.max_pushed_stream_id);
  Writer.next t.writer

let report_write_result t result = Writer.report_result t.writer result

let yield_writer t k = Writer.on_wakeup_writer t.writer k
