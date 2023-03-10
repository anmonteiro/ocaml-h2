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

module Writer = Serialize.Writer

type error =
  [ `Bad_request
  | `Internal_server_error
  | `Exn of exn
  ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> Body.Writer.t) -> unit

type response_state =
  | Waiting
  | Fixed of
      { response : Response.t
      ; mutable iovec :
          [ `String of string | `Bigstring of Bigstringaf.t ] Httpaf.IOVec.t
      }
  | Streaming of
      { response : Response.t
      ; response_body : Body.Writer.t
      ; trailers : Headers.t
      }
  | Complete of Response.t

type request_info =
  { request : Request.t
  ; request_body : Body.Reader.t
  ; mutable request_body_bytes : int64
  }

type active_state = (request_info, request_info) Stream.active_state

type active_stream =
  { body_buffer_size : int
  ; encoder : Hpack.Encoder.t
  ; mutable response_state : response_state
        (* We're not doing anything with these yet, we could probably have a
         * `Reqd.schedule_read_trailers` function that would be called once
         * trailer headers are emitted. *)
  ; mutable trailers_parser : Stream.partial_headers option
  ; mutable trailers : Headers.t option
  ; create_push_stream :
      Stream_identifier.t
      -> (t, [ `Push_disabled | `Stream_ids_exhausted ]) result
  }

and state =
  (active_state, active_stream, request_info * active_stream) Stream.state

and t = (state, error, error_handler) Stream.t

let create_active_request request request_body =
  { request; request_body; request_body_bytes = Int64.zero }

let create_active_stream encoder body_buffer_size create_push_stream =
  { body_buffer_size
  ; encoder
  ; response_state = Waiting
  ; trailers_parser = None
  ; trailers = None
  ; create_push_stream
  }

let request (t : t) =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _ | FullHeaders), _) ->
    assert false
  | Active ((Open (ActiveMessage { request; _ }) | HalfClosed { request; _ }), _)
  | Reserved ({ request; _ }, _) ->
    request
  | Closed _ -> assert false

let request_body (t : t) =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _ | FullHeaders), _) ->
    assert false
  | Active
      ( ( Open (ActiveMessage { request_body; _ })
        | HalfClosed { request_body; _ } )
      , _ ) ->
    request_body
  | Reserved _ ->
    (* From RFC7540§8.1:
     *   Promised requests MUST NOT include a request body. *)
    failwith
      "h2.Reqd.request_body: Promised requests must not include a request body"
  | Closed _ -> failwith "h2.Reqd.request_body: Stream already closed"

let response (t : t) =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) -> None
  | Active
      ( (Open (FullHeaders | ActiveMessage _) | HalfClosed _)
      , { response_state; _ } )
  | Reserved (_, { response_state; _ }) ->
    (match response_state with
    | Waiting -> None
    | Streaming { response; _ } | Fixed { response; _ } | Complete response ->
      Some response)
  | Closed _ -> None

let response_exn (t : t) =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) ->
    failwith "h2.Reqd.response_exn: response has not started"
  | Active
      ( (Open (FullHeaders | ActiveMessage _) | HalfClosed _)
      , { response_state; _ } )
  | Reserved (_, { response_state; _ }) ->
    (match response_state with
    | Waiting -> failwith "h2.Reqd.response_exn: response has not started"
    | Streaming { response; _ } | Fixed { response; _ } | Complete response ->
      response)
  | Closed _ -> assert false

let send_fixed_response (t : t) s response data =
  match s.response_state with
  | Waiting ->
    let iovec, length =
      match data with
      | `String s ->
        let len = String.length s in
        let iovec = { Httpaf.IOVec.buffer = `String s; off = 0; len } in
        iovec, len
      | `Bigstring b ->
        let len = Bigstringaf.length b in
        let iovec = { Httpaf.IOVec.buffer = `Bigstring b; off = 0; len } in
        iovec, len
    in
    let should_send_data = length <> 0 in
    let frame_info =
      Writer.make_frame_info
        ~max_frame_size:t.max_frame_size
        ~flags:
          (if should_send_data
           then Flags.default_flags
           else Flags.(set_end_stream default_flags))
        t.id
    in
    Writer.write_response_headers t.writer s.encoder frame_info response;
    (* From RFC7540§8.1:
     *   An HTTP request/response exchange fully consumes a single stream.
     *   [...] A response starts with a HEADERS frame and ends with a frame
     *   bearing END_STREAM, which places the stream in the "closed" state. *)
    if should_send_data
    then s.response_state <- Fixed { response; iovec }
    else s.response_state <- Complete response;
    Writer.wakeup t.writer
  | Streaming _ -> failwith "h2.Reqd.respond_with_*: response already started"
  | Fixed _ -> failwith "h2.Reqd.respond_with_*: response already sent"
  | Complete _ -> failwith "h2.Reqd.respond_with_*: response already complete"

let schedule_trailers (t : t) new_trailers =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) -> assert false
  | Closed _ -> failwith "h2.Reqd.schedule_trailers: stream already closed"
  | Reserved _ -> failwith "h2.Reqd.schedule_trailers: response not started"
  | Active ((Open (FullHeaders | ActiveMessage _) | HalfClosed _), stream) ->
    (match stream.response_state with
    | Streaming { response; response_body; trailers = old_trailers } ->
      if old_trailers <> Headers.empty
      then failwith "h2.Reqd.schedule_trailers: trailers already scheduled";
      stream.response_state <-
        Streaming { response; response_body; trailers = new_trailers }
    | _ ->
      failwith
        "h2.Reqd.schedule_trailers: can only send trailers in Streaming mode")

let unsafe_respond_with_data (t : t) response data =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) -> assert false
  | Active ((Open (FullHeaders | ActiveMessage _) | HalfClosed _), stream) ->
    send_fixed_response t stream response data
  | Reserved (request_info, stream) ->
    send_fixed_response t stream response data;
    (* From RFC7540§8.1:
     *   reserved (local): [...] In this state, only the following transitions
     *   are possible: The endpoint can send a HEADERS frame. This causes the
     *   stream to open in a "half-closed (remote)" state. *)
    Writer.flush t.writer (fun () ->
        t.state <- Active (HalfClosed request_info, stream))
  | Closed _ -> assert false

let respond_with_string (t : t) response str =
  match t.error_code with
  | No_error -> unsafe_respond_with_data t response (`String str)
  | _ ->
    failwith
      "h2.Reqd.respond_with_string: invalid state, currently handling error"

let respond_with_bigstring (t : t) response bstr =
  match t.error_code with
  | No_error -> unsafe_respond_with_data t response (`Bigstring bstr)
  | _ ->
    failwith
      "h2.Reqd.respond_with_bigstring: invalid state, currently handling error"

let send_streaming_response ~flush_headers_immediately (t : t) s response =
  let wait_for_first_flush = not flush_headers_immediately in
  match s.response_state with
  | Waiting ->
    let frame_info =
      Writer.make_frame_info ~max_frame_size:t.max_frame_size t.id
    in
    let response_body_buffer = Bigstringaf.create s.body_buffer_size in
    let response_body =
      Body.Writer.create response_body_buffer ~writer:t.writer
    in
    Writer.write_response_headers t.writer s.encoder frame_info response;
    if wait_for_first_flush then Writer.yield t.writer;
    s.response_state <-
      Streaming { response; response_body; trailers = Headers.empty };
    Writer.wakeup t.writer;
    response_body
  | Streaming _ ->
    failwith "h2.Reqd.respond_with_streaming: response already started"
  | Fixed _ | Complete _ ->
    failwith "h2.Reqd.respond_with_streaming: response already complete"

let unsafe_respond_with_streaming (t : t) ~flush_headers_immediately response =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) -> assert false
  | Active ((Open (FullHeaders | ActiveMessage _) | HalfClosed _), stream) ->
    send_streaming_response ~flush_headers_immediately t stream response
  | Reserved (request_info, stream) ->
    let response_body =
      send_streaming_response ~flush_headers_immediately t stream response
    in
    (* From RFC7540§8.1:
     *   reserved (local): [...] In this state, only the following transitions
     *   are possible: The endpoint can send a HEADERS frame. This causes the
     *   stream to open in a "half-closed (remote)" state. *)
    Writer.flush t.writer (fun () ->
        t.state <- Active (HalfClosed request_info, stream));
    response_body
  | Closed _ -> assert false

let respond_with_streaming (t : t) ?(flush_headers_immediately = false) response
  =
  match t.error_code with
  | No_error ->
    unsafe_respond_with_streaming ~flush_headers_immediately t response
  | _ ->
    failwith
      "h2.Reqd.respond_with_streaming: invalid state, currently handling error"

let start_push_stream (t : t) s request =
  match s.create_push_stream t.id with
  | Ok promised_reqd ->
    let frame_info =
      Writer.make_frame_info ~max_frame_size:t.max_frame_size t.id
    in
    Writer.write_push_promise
      t.writer
      s.encoder
      frame_info
      ~promised_id:promised_reqd.id
      request;
    let { encoder; body_buffer_size; create_push_stream; _ } = s in
    (* From RFC7540§8.2:
     *   Promised requests [...] MUST NOT include a request body. *)
    let request_info = create_active_request request Body.Reader.empty in
    let active_stream =
      create_active_stream encoder body_buffer_size create_push_stream
    in
    (* From RFC7540§8.2.1:
     *   Sending a PUSH_PROMISE frame creates a new stream and puts the stream
     *   into the "reserved (local)" state for the server and the "reserved
     *   (remote)" state for the client.
     *
     * Note: we do this before flushing the writer because request handlers
     * might immediately call one of the `respond_with` functions and expect
     * the stream to be in the `Reserved` state. *)
    promised_reqd.state <- Reserved (request_info, active_stream);
    Writer.wakeup t.writer;
    Ok promised_reqd
  | Error e ->
    Error (e :> [ `Push_disabled | `Stream_cant_push | `Stream_ids_exhausted ])

(* TODO: We could easily allow the priority of the PUSH request to be
 * configurable. We should allow users of this API to define the weight (maybe
 * not strictly), dependency on the current Reqd, and exclusivity *)
let unsafe_push (t : t) request =
  match t.state with
  | Idle | Active (Open (WaitingForPeer | PartialHeaders _), _) -> assert false
  | Active ((Open (FullHeaders | ActiveMessage _) | HalfClosed _), stream) ->
    start_push_stream t stream request
  (* Already checked in `push` *)
  | Reserved _ | Closed _ -> assert false

let push (t : t) request =
  match t.error_code with
  | No_error ->
    if Stream_identifier.is_pushed t.id
    then
      (* From RFC7540§6.6:
       *   PUSH_PROMISE frames MUST only be sent on a peer-initiated stream that
       *   is in either the "open" or "half-closed (remote)" state. *)
      Error `Stream_cant_push
    else unsafe_push t request
  | _ -> failwith "h2.Reqd.push: invalid state, currently handling error"

let _report_error ?request (t : t) s (error : error) error_code =
  match s.response_state, t.error_code with
  | Waiting, No_error ->
    t.error_code <- Stream.error_to_code error error_code;
    let status =
      match (error :> [ error | Status.standard ]) with
      | `Exn _ -> `Internal_server_error
      | #Status.standard as status -> status
    in
    t.error_handler ?request error (fun headers ->
        let response = Response.create ~headers status in
        unsafe_respond_with_streaming ~flush_headers_immediately:true t response)
  | Streaming { response_body; _ }, No_error ->
    Body.Writer.close response_body;
    t.error_code <- Stream.error_to_code error error_code;
    Stream.reset_stream t error_code
  | Fixed _, No_error ->
    (* Still need to send an RST_STREAM frame. Set t.error_code with
     * `error_code` and `flush_response_body` below will reset the stream after
     * flushing any remaining body bytes. *)
    t.error_code <- Stream.error_to_code error error_code;
    Stream.reset_stream t error_code
  | (Waiting | Fixed _ | Streaming _), Exn _ ->
    (* XXX(seliopou): Decide what to do in this unlikely case. There is an
     * outstanding call to the [error_handler], but an intervening exception
     * has been reported as well. *)
    failwith "h2.Reqd.report_exn: NYI"
  | (Waiting | Streaming _ | Fixed _ | Complete _), _ -> ()

let report_error (t : t) exn error_code =
  match t.state with
  | Idle | Reserved _ | Active (Open (WaitingForPeer | PartialHeaders _), _) ->
    assert false
  | Active (Open FullHeaders, stream) -> _report_error t stream exn error_code
  | Active
      ( ( Open (ActiveMessage { request; request_body; _ })
        | HalfClosed { request; request_body; _ } )
      , stream ) ->
    Body.Reader.close request_body;
    _report_error t stream ~request exn error_code
  | Closed _ -> ()

let report_exn t exn = report_error t (`Exn exn) Error_code.InternalError

let try_with t f : (unit, exn) Result.result =
  try
    f ();
    Ok ()
  with
  | exn ->
    report_exn t exn;
    Error exn

let error_code = Stream.error_code
(* Private API, not exposed to the user through h2.mli *)

let requires_output (t : t) =
  match t.state with
  | Idle -> false
  | Reserved _ -> true
  | Active (Open (WaitingForPeer | PartialHeaders _), _) -> false
  | Active
      ( (Open (FullHeaders | ActiveMessage _) | HalfClosed _)
      , { response_state; _ } ) ->
    (* From RFC7540§8.1:
     *   A server can send a complete response prior to the client sending an
     *   entire request if the response does not depend on any portion of the
     *   request that has not been sent and received. *)
    (match response_state with
    | Complete _ -> false
    | Fixed { iovec = { len; _ }; _ } -> len > 0
    | Streaming _ -> true
    | Waiting -> true)
  | Closed _ -> false

let flush_request_body (t : t) =
  match t.state with
  | Active
      ( ( Open (ActiveMessage { request_body; _ })
        | HalfClosed { request_body; _ } )
      , _ ) ->
    if Body.Reader.has_pending_output request_body
    then (
      try Body.Reader.execute_read request_body with exn -> report_exn t exn)
  | _ -> ()

let write_buffer_data writer ~off ~len frame_info buffer =
  match buffer with
  | `String str -> Writer.write_data writer ~off ~len frame_info str
  | `Bigstring bstr -> Writer.schedule_data writer ~off ~len frame_info bstr

let close_stream (t : t) =
  match t.error_code with
  | No_error ->
    (match t.state with
    | Active (Open (FullHeaders | ActiveMessage _), _) ->
      (* From RFC7540§8.1:
       *   A server can send a complete response prior to the client sending an
       *   entire request if the response does not depend on any portion of the
       *   request that has not been sent and received. When this is true, a
       *   server MAY request that the client abort transmission of a request
       *   without error by sending a RST_STREAM with an error code of NO_ERROR
       *   after sending a complete response (i.e., a frame with the END_STREAM
       *   flag). *)
      Stream.reset_stream t Error_code.NoError
    | Active (HalfClosed _, _) ->
      Writer.flush t.writer (fun () -> Stream.finish_stream t Finished)
    | _ -> assert false)
  | Exn _ -> Stream.reset_stream t InternalError
  | Other { code; _ } -> Stream.reset_stream t code

let flush_response_body (t : t) ~max_bytes =
  match t.state with
  | Active ((Open _ | HalfClosed _), stream) ->
    (match stream.response_state with
    | Streaming { response; response_body; trailers } ->
      if Body.Writer.has_pending_output response_body && max_bytes > 0
      then
        Body.Writer.transfer_to_writer
          response_body
          t.writer
          ~max_frame_size:t.max_frame_size
          ~max_bytes
          t.id
      else if Body.Writer.is_closed response_body
      then (
        (* no pending output and closed, we can finalize the message and close
           the stream *)
        let frame_info =
          Writer.make_frame_info
            ~max_frame_size:t.max_frame_size
            ~flags:Flags.(set_end_stream default_flags)
            t.id
        in
        match trailers with
        | _ :: _ ->
          Writer.write_response_trailers
            t.writer
            stream.encoder
            frame_info
            trailers;
          close_stream t;
          stream.response_state <- Complete response;
          0
        | [] ->
          (* From RFC7540§6.9.1:
           *   Frames with zero length with the END_STREAM flag set (that is,
           *   an empty DATA frame) MAY be sent if there is no available space
           *   in either flow-control window. *)
          Writer.schedule_data t.writer frame_info ~len:0 Bigstringaf.empty;
          close_stream t;
          stream.response_state <- Complete response;
          0)
      else (* no pending output but Body is still open *)
        0
    | Fixed ({ iovec = { buffer; off; len } as iovec; _ } as r)
      when max_bytes > 0 ->
      let is_partial_flush = max_bytes < len in
      let frame_info =
        let flags =
          if is_partial_flush
          then Flags.default_flags
          else Flags.(set_end_stream default_flags)
        in
        Writer.make_frame_info ~max_frame_size:t.max_frame_size ~flags t.id
      in
      let len_to_write = if is_partial_flush then max_bytes else len in
      write_buffer_data t.writer ~off ~len:len_to_write frame_info buffer;
      r.iovec <- Httpaf.IOVec.shift iovec len_to_write;
      if not is_partial_flush then close_stream t;
      len_to_write
    | Fixed _ | Waiting | Complete _ -> 0)
  | _ -> 0

let deliver_trailer_headers (t : t) headers =
  match t.state with
  | Active (Open (PartialHeaders _ | FullHeaders), _) -> assert false
  | Active ((Open (ActiveMessage _) | HalfClosed _), stream) ->
    (* TODO: call the schedule_trailers callback *)
    stream.trailers <- Some headers
  | _ -> assert false
