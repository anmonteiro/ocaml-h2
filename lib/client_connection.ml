(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    Copyright (c) 2019 Antonio N. Monteiro.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module AB = Angstrom.Buffered
module Reader = Parse.Reader
module Writer = Serialize.Writer
module Streams = Streams.Client_streams

type error =
  [ `Malformed_response of string
  | `Exn of exn
  ]

type response_handler = Response.t -> [`read] Body.t  -> unit
(* TODO: we'll need a connection-level error handler that terminates the
 * connection and one for each individual stream. *)
type error_handler = error -> unit

type reader_state =
  | New of Reader.server_connection_preface
  | Active of Reader.frame

(*
  ; request          : Request.t
  ; request_body     : [ `write ] Body.t
  ; response_handler : (Response.t -> [`read] Body.t -> unit)
  ; error_handler    : (error -> unit)
  ; state  : state ref
 *)

(* TODO: ping handler *)
(* TODO: connection-level error handler *)
type t =
  { settings         : Settings.t
  ; mutable reader   : reader_state
  ; writer           : Writer.t
  ; config           : Config.t
  ; streams          : Streams.t
  ; mutable current_stream_id : Stream_identifier.t
  ; mutable current_server_streams : int
  ; mutable receiving_headers_for_stream : Stream_identifier.t option
  ; mutable did_send_go_away : bool
  ; wakeup_writer  : (unit -> unit) ref
  ; wakeup_reader  : (unit -> unit) list ref
    (* From RFC7540§4.3:
         Header compression is stateful. One compression context and one
         decompression context are used for the entire connection. *)
  ; hpack_encoder : Hpack.Encoder.t
  ; hpack_decoder : Hpack.Decoder.t
  }

let is_active t =
  match t.reader with
  | Active _ -> true
  | New _ -> false

let reader t =
  match t.reader with
  | New reader -> reader
  | Active reader -> reader

let is_shutdown t =
  Reader.is_closed (reader t) && Writer.is_closed t.writer

let on_wakeup_reader t k =
  if is_shutdown t
  then failwith "on_wakeup_reader on closed conn"
  else t.wakeup_reader := k::!(t.wakeup_reader)

let on_wakeup_writer t k =
  if is_shutdown t
  then failwith "on_wakeup_writer on closed conn"
  else t.wakeup_writer := k

let default_wakeup_writer = fun () -> ()

let _wakeup_writer wakeup_ref =
  let f = !wakeup_ref in
  wakeup_ref := default_wakeup_writer;
  f ()

let wakeup_writer t =
  _wakeup_writer t.wakeup_writer

let wakeup_reader t =
  let fs = !(t.wakeup_reader) in
  t.wakeup_reader := [];
  List.iter (fun f -> f ()) fs

let shutdown_reader t =
  Reader.force_close (reader t);
  ()
  (* begin match !(t.state) with
  | Awaiting_response | Closed -> ()
  | Received_response(_, response_body) ->
    Body.close_reader response_body;
    Body.execute_read response_body;
  end *)

let flush_request_body _t =
  (* if Body.has_pending_output t.request_body then
    Body.transfer_to_writer t.request_body t.writer
      ~max_frame_size:t.settings.max_frame_size *)
  ()

let shutdown_writer t =
  flush_request_body t;
  Writer.close t.writer;
  ()
  (* Body.close_writer t.request_body *)

let shutdown t =
  shutdown_reader t;
  shutdown_writer t

let handle_error t = function
  | Error.ConnectionError (error, data) ->
    if not t.did_send_go_away then begin
      (* From RFC7540§5.4.1:
           An endpoint that encounters a connection error SHOULD first send a
           GOAWAY frame (Section 6.8) with the stream identifier of the last
           stream that it successfully received from its peer. The GOAWAY frame
           includes an error code that indicates why the connection is
           terminating. After sending the GOAWAY frame for an error condition,
           the endpoint MUST close the TCP connection. *)
      let debug_data = if String.length data == 0 then
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
        ~last_stream_id:(if Stream_identifier.(t.current_stream_id === -1l) then
          Stream_identifier.connection
        else
          t.current_stream_id)
        error;
      Writer.flush t.writer (fun () ->
        (* XXX: We need to allow lower numbered streams to complete before
         * shutting down. *)
        shutdown t);
      t.did_send_go_away <- true;
      wakeup_writer t
    end
  | StreamError (stream_id, error) ->
    begin match Streams.find t.streams stream_id with
    | Some reqd ->
      Respd.reset_stream reqd error;
    | None ->
      (* Possible if the stream was going to enter the Idle state (first time
       * we saw e.g. a PRIORITY frame for it) but had e.g. a
       * FRAME_SIZE_ERROR. *)
      let frame_info = Writer.make_frame_info stream_id in
      Writer.write_rst_stream t.writer frame_info error;
    end;
    wakeup_writer t

let report_connection_error t ?(additional_debug_data="") error =
  handle_error t (ConnectionError (error, additional_debug_data))

let report_stream_error t stream_id error =
  handle_error t (StreamError (stream_id, error))

let set_error_and_handle t stream error error_code =
  Respd.report_error stream error error_code;
  wakeup_writer t

let report_exn t exn =
  if not (is_shutdown t) then begin
    let additional_debug_data = Printexc.to_string exn in
    report_connection_error t ~additional_debug_data Error.InternalError
  end

let handle_headers t ~end_stream respd headers =
  (* From RFC7540§5.1.2:
       Endpoints MUST NOT exceed the limit set by their peer. An endpoint that
       receives a HEADERS frame that causes its advertised concurrent stream
       limit to be exceeded MUST treat this as a stream error (Section 5.4.2)
       of type PROTOCOL_ERROR or REFUSED_STREAM. *)
  if t.current_server_streams + 1 > t.settings.max_concurrent_streams then
    (* From RFC7540§8.1.4:
         The REFUSED_STREAM error code can be included in a RST_STREAM frame to
         indicate that the stream is being closed prior to any processing
         having occurred. Any request that was sent on the reset stream can be
         safely retried. *)
    report_stream_error t respd.Respd.id Error.RefusedStream
  else begin
    (* From RFC7540§5.1.2:
         Streams that are in the "open" state or in either of the "half-closed"
         states count toward the maximum number of streams that an endpoint is
         permitted to open. *)
    let active_stream =
      Reqd.create_active_stream
        t.hpack_encoder
        t.config.response_body_buffer_size
    in
    respd.state <- Open FullHeaders;
    t.receiving_headers_for_stream <- None;
    t.current_server_streams <- t.current_server_streams + 1;
    (* From RFC7540§8.1.2.4:
         For HTTP/2 responses, a single :status pseudo-header field is defined
         that carries the HTTP status code field (see [RFC7231], Section 6).
         This pseudo-header field MUST be included in all responses; otherwise,
         the response is malformed (Section 8.1.2.6). *)
    match Headers.get_multi_pseudo headers "method" with
    | [ status ] ->
      begin match end_stream, Message.unique_content_length_values headers with
      | true, [ content_length ]
        when
          Int64.compare
            (Message.content_length_of_string content_length)
            0L
          != 0 ->
        (* From RFC7540§8.1.2.6:
             A request or response is also malformed if the value of a
             content-length header field does not equal the sum of the DATA
             frame payload lengths that form the body. *)
        let message =
          "The value of the content-length header field does not equal the length of the body"
        in
        set_error_and_handle t respd (`Malformed_response message) ProtocolError;
      | _ ->
        let response = Response.create ~headers (Status.of_string status) in
        let response_body = if end_stream then
          Body.empty
        else
          (* TODO: Initializing the response body with an empty bigstring trades
           * an allocation here vs. when the first data frame arrives. This
           * would probably make sense if this were HTTP/1 because we wouldn't
           * know whether the response had a body or not, but in HTTP/2 we
           * clearly do. It's probably fine to just allocate here if we know
           * we're gonna get data. *)
          Body.create Bigstringaf.empty
        in
        let response_info =
          { Respd
          . response
          ; response_body
          }
        in
        if end_stream then begin
          (* From RFC7540§5.1:
               [...] an endpoint receiving an END_STREAM flag causes the stream
               state to become "half-closed (remote)". *)
          respd.state <- HalfClosed new_stream_state;
          (* Deliver EOF to the request body, as the handler might be waiting
           * on it to produce a response. *)
          Body.close_reader response_body;
        end else begin
          respd.state <- Open (ActiveRequest new_stream_state);
        end;
        t.request_handler reqd;
        wakeup_writer t
      end
    | _ ->
      (* From RFC7540§8.1.2.6:
           For malformed requests, a server MAY send an HTTP response prior to
           closing or resetting the stream. *)
      let message =
        "HTTP/2 responses must include a single `:status` pseudo-header"
      in
      set_error_and_handle t respd (`Malformed_response message) ProtocolError;
  end

let handle_headers_block t respd partial_headers flags headers_block =
  let open AB in
  let end_headers = Flags.test_end_header flags in
  (* From RFC7540§6.10:
       An endpoint receiving HEADERS, PUSH_PROMISE, or CONTINUATION
       frames needs to reassemble header blocks and perform decompression
       even if the frames are to be discarded *)
  let parse_state' =
    AB.feed partial_headers.Stream.parse_state (`Bigstring headers_block)
  in
  if end_headers then begin
    let parse_state' = AB.feed parse_state' `Eof in
    match parse_state' with
    | Done (_, Ok headers) ->
      (* `handle_headers` will take care of transitioning the stream state *)
      handle_headers t ~end_stream:partial_headers.end_stream respd headers
    (* From RFC7540§4.3:
         A decoding error in a header block MUST be treated as a connection
         error (Section 5.4.1) of type COMPRESSION_ERROR. *)
    | Done (_, Error _)
    | Partial _ ->
      report_connection_error t Error.CompressionError
    | Fail (_, _, message) ->
      report_connection_error t ~additional_debug_data:message Error.CompressionError
  end else
    partial_headers.parse_state <- parse_state'

(* TODO: reprioritize stream? *)
let handle_first_response_bytes t respd frame_header ?priority:_ headers_block =
  let open Streams in
  let { Frame.flags; stream_id; _ } = frame_header in
  let end_headers = Flags.test_end_header flags in
  let headers_block_length = Bigstringaf.length headers_block in
  let initial_buffer_size = if end_headers then
    headers_block_length
  else
    (* Conservative estimate that there's only going to be one CONTINUATION
     * frame. *)
    2 * headers_block_length
  in
  let partial_headers =
    { Stream
    . parse_state = AB.parse ~initial_buffer_size (Hpack.Decoder.decode_headers t.hpack_decoder)
    ; end_stream = Flags.test_end_stream flags
    }
  in
  respd.Respd.state <- Open (PartialHeaders partial_headers);
  if not end_headers then
    t.receiving_headers_for_stream <- Some stream_id;
  handle_headers_block t respd partial_headers flags headers_block

let process_headers_frame t { Frame.frame_header; _ } ?priority headers_block =
  let { Frame.stream_id; _ } = frame_header in
  match priority with
  | Some { Priority.stream_dependency; _ }
    when Stream_identifier.(stream_dependency === stream_id) ->
    (* From RFC7540§5.3.1:
         A stream cannot depend on itself. An endpoint MUST treat this as a
         stream error (Section 5.4.2) of type PROTOCOL_ERROR. *)
    report_stream_error t stream_id Error.ProtocolError
  | _ ->
    match Streams.find t.streams stream_id with
    | None ->
      (* TODO: What does it mean to receive a response for a stream that's no
       * longer in the priority tree? Either:
       * 1. we canceled a request and the response was already in flight.
       * 2. we're getting a response for a request we didn't send?! *)
      assert false
    | Some respd ->
      match respd.state with
      | Idle ->
        (* From RFC7540§6.2:
             HEADERS frames can be sent on a stream in the "idle", "reserved
             (local)", "open", or "half-closed (remote)" state. *)
        report_connection_error t Error.ProtocolError
      | Open Awaiting_response ->
        handle_first_response_bytes t respd frame_header ?priority headers_block
      | Open (PartialHeaders _) ->
          assert false
      (* if we're getting a HEADERS frame at this point, they must be
       * trailers, and the END_STREAM flag needs to be set. *)
      | Open (FullHeaders rs) ->
        process_trailer_headers t respd rs frame_header headers_block
      | Open (ActiveRequest rs) ->
        process_trailer_headers t respd rs frame_header headers_block
      | HalfClosed _
        (* From RFC7540§5.1:
             half-closed (remote): [...] If an endpoint receives additional
             frames, other than WINDOW_UPDATE, PRIORITY, or RST_STREAM, for a
             stream that is in this state, it MUST respond with a stream
             error (Section 5.4.2) of type STREAM_CLOSED. *)
      | Closed { reason = ResetByThem _; _ } ->
        (* From RFC7540§5.1:
             closed: [...] An endpoint that receives any frame other than
             PRIORITY after receiving a RST_STREAM MUST treat that as a
             stream error (Section 5.4.2) of type STREAM_CLOSED. *)
        report_stream_error t stream_id Error.StreamClosed
      (* From RFC7540§5.1:
           reserved (local): [...] Receiving any type of frame other than
           RST_STREAM, PRIORITY, or WINDOW_UPDATE on a stream in this state
           MUST be treated as a connection error (Section 5.4.1) of type
           PROTOCOL_ERROR. *)
      | Reserved _
      | Closed _ ->
        (* From RFC7540§5.1:
             Similarly, an endpoint that receives any frames after receiving
             a frame with the END_STREAM flag set MUST treat that as a
             connection error (Section 5.4.1) of type STREAM_CLOSED [...]. *)
        report_connection_error t Error.StreamClosed

let process_data_frame t { Frame.frame_header; _ } bstr =
  ()

let process_priority_frame t { Frame.frame_header; _ } priority =
  ()

let process_rst_stream_frame t { Frame.frame_header; _ } error_code =
  ()

let process_settings_frame t { Frame.frame_header; _ } settings =
  let open Streams in
  (* TODO: what to do in the case of receiving an acked SETTINGS frame for
   * settings we didn't send? *)
  let { Frame.flags; _ } = frame_header in
  (* We already checked that an acked SETTINGS is empty. Don't need to do
   * anything else in that case *)
  if not Flags.(test_ack flags) then begin
    match Settings.check_settings_list settings with
    | None ->
      (* From RFC7540§6.5:
           Each parameter in a SETTINGS frame replaces any existing value for that
           parameter. Parameters are processed in the order in which they appear,
           and a receiver of a SETTINGS frame does not need to maintain any state
           other than the current value of its parameters. *)
      List.iter (function
        | Settings.HeaderTableSize, x ->
          (* From RFC7540§6.5.2:
               Allows the sender to inform the remote endpoint of the maximum
               size of the header compression table used to decode header
               blocks, in octets. *)
          t.settings.header_table_size <- x;
          Hpack.Encoder.set_capacity t.hpack_encoder x;
        | EnablePush, x ->
          (* We've already verified that this setting is either 0 or 1 in the
           * call to `Settings.check_settings_list` above. *)
          t.settings.enable_push <- x == 1
        | MaxConcurrentStreams, x ->
          t.settings.max_concurrent_streams <- x
        | InitialWindowSize, new_val ->
          (* From RFC7540§6.9.2:
               [...] a SETTINGS frame can alter the initial flow-control window
               size for streams with active flow-control windows (that is,
               streams in the "open" or "half-closed (remote)" state). When the
               value of SETTINGS_INITIAL_WINDOW_SIZE changes, a receiver MUST
               adjust the size of all stream flow-control windows that it
               maintains by the difference between the new value and the old
               value. *)
          let old_val = t.settings.initial_window_size in
          t.settings.initial_window_size <- new_val;
          let growth = new_val - old_val in
          let exception Local in
          begin match Streams.iter ~f:(fun stream ->
              (* From RFC7540§6.9.2:
                   An endpoint MUST treat a change to
                   SETTINGS_INITIAL_WINDOW_SIZE that causes any flow-control
                   window to exceed the maximum size as a connection error
                   (Section 5.4.1) of type FLOW_CONTROL_ERROR. *)
              if not (Streams.add_flow stream growth) then
                raise Local)
            t.streams
          with
          | () -> ()
          | exception Local ->
            report_connection_error t
              ~additional_debug_data:(Format.sprintf
                "Window size for stream would exceed %d"
                Settings.WindowSize.max_window_size)
              Error.FlowControlError
          end
        | MaxFrameSize, x ->
          t.settings.max_frame_size <- x;
          Streams.iter ~f:(fun (Stream { streamd; _ }) ->
            if Respd.requires_output streamd then
              streamd.max_frame_size <- x)
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
           ACK (0x1): [...] When this bit is set, the payload of the SETTINGS
           frame MUST be empty. *)
      Writer.write_settings t.writer frame_info [];
      wakeup_writer t
    | Some error -> handle_error t error
  end

let process_push_promise_frame t frame promised_stream_id headers_block =
  ()

let process_ping_frame t { Frame.frame_header; _ } payload =
  ()

let process_goaway_frame t _frame payload =
  ()

let process_window_update_frame t { Frame.frame_header; _ } window_increment =
  ()

let process_continuation_frame t { Frame.frame_header; _ } headers_block =
  ()

(* Unlike e.g. http/af where a new connection is created per request, we create
   a single connection where all requests go through. HTTP/2 allows concurrency
   to exist on the connection level.

   From RFC7540§1:
     HTTP/2 [...] allows interleaving of request and response messages on the
     same connection and uses an efficient coding for HTTP header fields. *)
let create ?(config=Config.default) =
  let settings =
    { Settings
    . default_settings
    with max_frame_size = config.read_buffer_size
    ; max_concurrent_streams = config.max_concurrent_streams
    ; initial_window_size = config.initial_window_size
    ; enable_push = config.enable_server_push
    }
  in
  let rec preface_handler = fun recv_frame settings_list ->
    let t = Lazy.force t in
    (* Now process the client's SETTINGS frame. `process_settings_frame` will
     * take care of calling `wakeup_writer`. *)
    process_settings_frame t recv_frame settings_list
  and frame_handler t = function
    | Error e ->
      handle_error t e
    | Ok ({ Frame.frame_payload; frame_header } as frame) ->
      match t.receiving_headers_for_stream with
      | Some stream_id
        when not (Stream_identifier.(stream_id === frame_header.stream_id)) ||
             frame_header.frame_type != Continuation ->
        (* From RFC7540§6.2:
             A HEADERS frame without the END_HEADERS flag set MUST be followed
             by a CONTINUATION frame for the same stream. A receiver MUST treat
             the receipt of any other type of frame or a frame on a different
             stream as a connection error (Section 5.4.1) of type
             PROTOCOL_ERROR. *)
        report_connection_error t
          ~additional_debug_data:"HEADERS or PUSH_PROMISE without the \
END_HEADERS flag set must be followed by a CONTINUATION frame for the same \
stream"
          Error.ProtocolError
      | _ ->
        match frame_payload with
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
               Frames of unknown types are ignored. *)
          ()
  and t = lazy
    { settings
    ; config = config
      (* From RFC7540§5.1.1:
           Streams initiated by a client MUST use odd-numbered stream
           identifiers *)
    ; current_stream_id = -1l
    ; current_server_streams = 0
    ; did_send_go_away = false
    ; receiving_headers_for_stream = None
    ; reader = New (Reader.server_connection_preface preface_handler)
    (* TODO: `response_buffer_size`?! *)
    ; writer = Writer.create Config.default.response_buffer_size
    ; streams = Streams.make_root ()
    ; wakeup_writer             = ref default_wakeup_writer
    ; wakeup_reader             = ref []
    (* From RFC7540§4.3:
         Header compression is stateful. One compression context and one
         decompression context are used for the entire connection. *)
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
  if t.settings.initial_window_size > Settings.default_settings.initial_window_size then begin
    let _diff =
      t.settings.initial_window_size - Settings.default_settings.initial_window_size
    in
    ()
    (* FIXME: *)
    (* send_window_update t t.streams diff *)
  end;
  t

let request t request ~error_handler ~response_handler =
  let max_frame_size = t.settings.max_frame_size in
  let request_body = Body.create (Bigstringaf.create max_frame_size) in
  t.current_stream_id <- Int32.add t.current_stream_id 2l;
  let stream_id = t.current_stream_id in
  let respd =
    Respd.create
      stream_id
      request
      request_body
      ~max_frame_size
      t.writer
      error_handler
      response_handler
      ignore
  in
  (* TODO: priority *)
  Streams.add t.streams
    (* ?priority *)
    ~initial_window_size:t.settings.initial_window_size
    respd;
  let frame_info =
    Writer.make_frame_info
      ~max_frame_size:max_frame_size
      ~flags:Flags.default_flags
      stream_id
  in
  Writer.write_request_headers t.writer t.hpack_encoder frame_info request;
  Writer.flush t.writer (fun () ->
    respd.state <- Open Awaiting_response);
  wakeup_writer t;
  (* TODO: closing the request body puts the stream on half-closed (local)
   * state *)
  request_body

let flush_response_body t =
  match !(t.state) with
  | Awaiting_response | Closed -> ()
  | Received_response(_, response_body) ->
    try Body.execute_read response_body
    with exn -> report_exn t exn

let _next_read_operation t =
  match !(t.state) with
  | Awaiting_response | Closed -> Reader.next t.reader
  | Received_response(_, response_body) ->
    if not (Body.is_closed response_body)
    then Reader.next t.reader
    else begin
      Reader.force_close t.reader;
      Reader.next        t.reader
    end

let next_read_operation t =
  match _next_read_operation t with
  | `Error (`Parse(marks, message)) ->
    let message = String.concat "" [ String.concat ">" marks; ": "; message] in
    set_error_and_handle t (`Malformed_response message);
    `Close
  | `Error (`Invalid_response_body_length _ as error) ->
    set_error_and_handle t error;
    `Close
  | (`Read | `Close) as operation -> operation

let read_with_more t bs ~off ~len more =
  let consumed = Reader.read_with_more t.reader bs ~off ~len more in
  flush_response_body t;
  consumed

let read t bs ~off ~len =
  read_with_more t bs ~off ~len Incomplete

let read_eof t bs ~off ~len =
  read_with_more t bs ~off ~len Complete

let next_write_operation t =
  flush_request_body t;
  Writer.next t.writer

let yield_writer t k =
  if Body.is_closed t.request_body
  then begin
    Writer.close t.writer;
    k ()
  end else
    Body.when_ready_to_write t.request_body k

let report_write_result t result =
  Writer.report_result t.writer result

let is_closed t = Reader.is_closed t.reader && Writer.is_closed t.writer
