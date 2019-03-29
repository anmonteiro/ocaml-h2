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

module Reader = Parse.Reader
module Writer = Serialize.Writer

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Exn of exn
  ]

type response_handler = Response.t -> [`read] Body.t  -> unit
type error_handler = error -> unit

type reader_state =
    (* TODO: consider getting rid of this state *)
  | Uninitialized
  | New of Reader.frame
  | Active of Reader.frame

type state =
  | Awaiting_response
  | Received_response of Response.t * [`read] Body.t
  | Closed

type t =
  { settings         : Settings.t
  ; request          : Request.t
  ; request_body     : [ `write ] Body.t
  ; response_handler : (Response.t -> [`read] Body.t -> unit)
  ; error_handler    : (error -> unit)
  ; mutable reader   : reader_state
  ; writer : Writer.t
  ; state  : state ref
  ; mutable error_code : [ `Ok | error ]
  ; mutable current_stream_id : Stream_identifier.t
  ; mutable did_send_go_away : bool
  ; wakeup_writer  : (unit -> unit) ref
  ; wakeup_reader  : (unit -> unit) list ref
    (* From RFC7540§4.3:
         Header compression is stateful. One compression context and one
         decompression context are used for the entire connection. *)
  ; encoder : Hpack.Encoder.t
  ; decoder : Hpack.Decoder.t
  }

let is_active t =
  match t.reader with
  | Active _ -> true
  | New _ -> false
  | Uninitialized -> false

let reader t =
  match t.reader with
  | Uninitialized -> assert false
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
  begin match !(t.state) with
  | Awaiting_response | Closed -> ()
  | Received_response(_, response_body) ->
    Body.close_reader response_body;
    Body.execute_read response_body;
  end

let flush_request_body t =
  (* if Body.has_pending_output t.request_body then
    Body.transfer_to_writer t.request_body t.writer
      ~max_frame_size:t.settings.max_frame_size *)
  ()

let shutdown_writer t =
  flush_request_body t;
  Writer.close t.writer;
  Body.close_writer t.request_body

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
      Reqd.reset_stream reqd error;
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
          Hpack.Encoder.set_capacity t.encoder x;
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
          Streams.iter ~f:(fun (Stream { reqd; _ }) ->
            if Reqd.requires_output reqd then
              reqd.max_frame_size <- x)
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

let create ?(config=Config.default) request ~error_handler ~response_handler =
  let state = ref Awaiting_response in
  let request_method = request.Request.meth in
  let handler response body =
    state := Received_response(response, body);
    response_handler response body
  in
  let request_body =
    Body.create (Bigstringaf.create config.request_body_buffer_size)
  in
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
      | PushPromise _ ->
        (* From RFC7540§8.2:
             A client cannot push. Thus, servers MUST treat the receipt of a
             PUSH_PROMISE frame as a connection error (Section 5.4.1) of type
             PROTOCOL_ERROR. *)
        report_connection_error
          t ~additional_debug_data:"Client cannot push" Error.ProtocolError
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
    ; request
    ; request_body
    ; response_handler
    ; error_handler
    ; error_code = `Ok
      (* From RFC7540§5.1.1:
           Streams initiated by a client MUST use odd-numbered stream
           identifiers *)
    ; current_stream_id = -1l
    ; did_send_go_away = false
    ; reader = Uninitialized
    ; writer = Writer.create Config.default.response_buffer_size
    ; state
    ; wakeup_writer             = ref default_wakeup_writer
    ; wakeup_reader             = ref []
    (* From RFC7540§4.3:
         Header compression is stateful. One compression context and one
         decompression context are used for the entire connection. *)
    ; encoder = Hpack.Encoder.(create settings.header_table_size)
    ; decoder = Hpack.Decoder.(create settings.header_table_size)
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
    t.reader <- New (Reader.frame (frame_handler t)));

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
  let state = ref Awaiting_response in
  let request_method = request.Request.meth in
  let handler response body =
    state := Received_response(response, body);
    response_handler response body
  in
  let request_body = Body.create (Bigstringaf.create config.request_body_buffer_size) in
  let t =
    { request
    ; request_body
    ; response_handler
    ; error_handler
    ; error_code = `Ok
    ; reader = Reader.response ~request_method handler
    ; writer = Writer.create ()
    ; state }
  in
  Writer.write_request t.writer request;
  request_body, t

let set_error_and_handle t error =
  shutdown t;
  t.state := Closed;
  t.error_code <- (error :> [`Ok | error]);
  t.error_handler error

let report_exn t exn =
  set_error_and_handle t (`Exn exn)

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
