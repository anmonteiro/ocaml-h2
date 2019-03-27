module Writer = Serialize.Writer
module AB = Angstrom.Buffered

type error =
  [ `Bad_request | `Internal_server_error | `Exn of exn ]

type error_handler =
  ?request:Request.t -> error -> (Headers.t -> [`write] Body.t) -> unit

type fixed_response =
  { response: Response.t
  ; mutable iovec:
      [ `String of string
      | `Bigstring of Bigstringaf.t
      ] Httpaf.IOVec.t
  }

type response_state =
  | Waiting of (unit -> unit) ref
  | Fixed of fixed_response
  | Streaming of Response.t * [`write] Body.t
  | Complete of Response.t

type active_request =
  { request                      : Request.t
  ; request_body                 : [`read] Body.t
  ; mutable request_body_bytes   : int64
  }

type partial_headers =
  { mutable parse_state : (Headers.t, Hpack.error) result AB.state
  ; end_stream          : bool
  }

type closed_reason =
  | Finished
  (* TODO: we could abide by the following by either 1) having I/O runtime
     support for timers or 2) by simply counting the number of frames received
     after we've sent an RST_STREAM?

     From RFC7540§5.4.2:
       Normally, an endpoint SHOULD NOT send more than one RST_STREAM frame for
       any stream. However, an endpoint MAY send additional RST_STREAM frames
       if it receives frames on a closed stream after more than a round-trip
       time. This behavior is permitted to deal with misbehaving
       implementations. *)
  | ResetByUs of Error.error_code
    (* Received an RST_STREAM frame from the peer. *)
  | ResetByThem of Error.error_code

type 'a active_stream =
  { request_state                : 'a
  ; response_body_buffer_size    : int
  ; encoder                      : Hpack.Encoder.t
  ; mutable response_state       : response_state
  ; mutable wait_for_first_flush : bool
    (* We're not doing anything with these yet, we could probably have a
     * `Reqd.schedule_read_trailers` function that would be called once trailer
     * headers are emitted. *)
  ; mutable trailers_parser      : partial_headers option
  ; mutable trailers             : Headers.t option
  ; create_push_stream           : unit -> (t * (unit -> unit), string) result
  }

and open_state =
  | PartialHeaders of partial_headers
  | FullHeaders of unit active_stream
  | ActiveRequest of active_request active_stream

and stream_state =
  | Idle
  | Open       of open_state
  | HalfClosed of active_request active_stream
  | Closed     of
    { reason : closed_reason
      (* When a stream is closed, we may want to keep it around in the hash
       * table for a while (e.g. to know whether this stream was reset by the
       * peer - some error handling code depends on that). We start with a
       * default value, and on every writer yield we decrement it. If it
       * reaches 0, the stream is finally removed from the hash table. *)
    ; mutable ttl : int
    }
  | Reserved   of active_request active_stream

and t =
  { id                     : Stream_identifier.t
  ; writer                 : Writer.t
  ; error_handler          : error_handler
  ; mutable error_code     : [`Ok | error ] * Error.error_code option
  ; mutable stream_state   : stream_state
    (* The largest frame payload we're allowed to write. *)
  ; mutable max_frame_size : int
  ; on_stream_closed       : unit -> unit
  }

let default_waiting = Sys.opaque_identity (fun () -> ())
let initial_ttl = 10

let create_active_request active_stream request request_body =
  { active_stream with
    request_state =
      { request
      ; request_body
      ; request_body_bytes = Int64.zero
      }
  }

let create_active_stream encoder response_body_buffer_size create_push_stream =
  { request_state = ()
  ; response_body_buffer_size
  ; encoder
  ; response_state  = Waiting (ref default_waiting)
  ; wait_for_first_flush = true
  ; trailers_parser = None
  ; trailers = None
  ; create_push_stream
  }

let create id ~max_frame_size writer error_handler on_stream_closed =
  { id
  ; writer
  ; error_handler
  ; stream_state   = Idle
  ; error_code     = `Ok, None
  ; max_frame_size
  ; on_stream_closed
  }

let done_waiting when_done_waiting =
  let f = !when_done_waiting in
  when_done_waiting := default_waiting;
  f ()

let request t =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _)
  | Open (FullHeaders _) -> assert false
  | Open (ActiveRequest { request_state = { request; _ }; _ })
  | HalfClosed { request_state = { request; _ }; _ }
  | Reserved { request_state = { request; _ }; _ } -> request
  | Closed _ -> assert false

let request_body t =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _)
  | Open (FullHeaders _) -> assert false
  | Open (ActiveRequest { request_state = { request_body; _ }; _ })
  | HalfClosed { request_state = { request_body; _ }; _ } ->
    request_body
  | Reserved _ ->
    (* From RFC7540§8.1:
         Promised requests MUST NOT include a request body. *)
    failwith "http2af.Reqd.request_body: Promised requests must not include a request body"
  | Closed _ -> assert false

let response t =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders { response_state; _ })
  | Open (ActiveRequest { response_state; _ })
  | HalfClosed { response_state; _ }
  | Reserved { response_state; _ } ->
    begin match response_state with
    | Waiting _ -> None
    | Streaming(response, _)
    | Fixed { response; _ }
    | Complete response -> Some response
    end
  | Closed _ -> assert false

let response_exn t =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _) ->
    failwith "http2af.Reqd.response_exn: response has not started"
  | Open (FullHeaders { response_state; _ })
  | Open (ActiveRequest { response_state; _ })
  | HalfClosed { response_state; _ }
  | Reserved { response_state; _ } ->
    begin match response_state with
    | Waiting _ -> failwith "http2af.Reqd.response_exn: response has not started"
    | Streaming(response, _)
    | Fixed { response; _ }
    | Complete response -> response
    end
  | Closed _ -> assert false

let send_fixed_response t s response data =
  match s.response_state with
  | Waiting when_done_waiting ->
    let iovec, length = match data with
    | `String s ->
      let len = String.length s in
      let iovec = { Httpaf.IOVec.buffer = `String s ; off = 0 ; len } in
      iovec, len
    | `Bigstring b ->
      let len = Bigstringaf.length b in
      let iovec = { Httpaf.IOVec.buffer = `Bigstring b ; off = 0 ; len } in
      iovec, len
    in
    let should_send_data = length != 0 in
    let frame_info =
      Writer.make_frame_info
        ~max_frame_size:t.max_frame_size
        ~flags:(if should_send_data then
          Flags.default_flags
        else
          Flags.(set_end_stream default_flags))
        t.id
    in
    Writer.write_response_headers t.writer s.encoder frame_info response;
    (* From RFC7540§8.1:
         An HTTP request/response exchange fully consumes a single stream.
         [...] A response starts with a HEADERS frame and ends with a frame
         bearing END_STREAM, which places the stream in the "closed" state. *)
    if should_send_data then
      s.response_state <- Fixed
        { response
        ; iovec
        }
    else
      s.response_state <- Complete response;
    done_waiting when_done_waiting;
  | Streaming _ ->
    failwith "http2af.Reqd.respond_with_*: response already started"
  | Fixed _
  | Complete _ ->
    failwith "http2af.Reqd.respond_with_*: response already complete"

let unsafe_respond_with_data t response data =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders stream) ->
    send_fixed_response t stream response data;
  | Open (ActiveRequest stream)
  | HalfClosed stream ->
    send_fixed_response t stream response data;
  | Reserved stream ->
    send_fixed_response t stream response data;
    (* From RFC7540§8.1:
         reserved (local): [...] In this state, only the following transitions
         are possible: The endpoint can send a HEADERS frame. This causes the
         stream to open in a "half-closed (remote)" state. *)
    Writer.flush t.writer (fun () ->
      t.stream_state <- HalfClosed stream)
  | Closed _ -> assert false

let respond_with_string t response str =
  if fst t.error_code <> `Ok then
    failwith "http2af.Reqd.respond_with_string: invalid state, currently handling error";
  unsafe_respond_with_data t response (`String str)

let respond_with_bigstring t response bstr =
  if fst t.error_code <> `Ok then
    failwith "http2af.Reqd.respond_with_bigstring: invalid state, currently handling error";
  unsafe_respond_with_data t response (`Bigstring bstr)

let send_streaming_response ~flush_headers_immediately t s response =
  s.wait_for_first_flush <- not flush_headers_immediately;
  match s.response_state with
  | Waiting when_done_waiting ->
    let frame_info =
      Writer.make_frame_info ~max_frame_size:t.max_frame_size t.id
    in
    let response_body_buffer = Bigstringaf.create s.response_body_buffer_size
    in
    let response_body = Body.create response_body_buffer in
    Writer.write_response_headers t.writer s.encoder frame_info response;
    if s.wait_for_first_flush then Writer.yield t.writer;
    s.response_state <- Streaming(response, response_body);
    done_waiting when_done_waiting;
    response_body
  | Streaming _ ->
    failwith "http2af.Reqd.respond_with_streaming: response already started"
  | Fixed _
  | Complete _ ->
    failwith "http2af.Reqd.respond_with_streaming: response already complete"

let unsafe_respond_with_streaming ~flush_headers_immediately t response =
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders s) ->
    send_streaming_response ~flush_headers_immediately t s response
  | Open (ActiveRequest s)
  | HalfClosed s ->
    send_streaming_response ~flush_headers_immediately t s response
  | Reserved s ->
    let response_body =
      send_streaming_response ~flush_headers_immediately t s response
    in
    (* From RFC7540§8.1:
         reserved (local): [...] In this state, only the following transitions
         are possible: The endpoint can send a HEADERS frame. This causes the
         stream to open in a "half-closed (remote)" state. *)
    Writer.flush t.writer (fun () ->
      t.stream_state <- HalfClosed s);
    response_body
  | Closed _ -> assert false

let respond_with_streaming ?(flush_headers_immediately=false) t response =
  if fst t.error_code <> `Ok then
    failwith "http2af.Reqd.respond_with_streaming: invalid state, currently handling error";
  unsafe_respond_with_streaming ~flush_headers_immediately t response

let start_push_stream t s request =
  match s.create_push_stream () with
  | Ok (promised_reqd, wakeup_writer) ->
    let frame_info =
      Writer.make_frame_info ~max_frame_size:t.max_frame_size t.id
    in
    Writer.write_push_promise
      t.writer
      s.encoder
      frame_info
      ~promised_id:promised_reqd.id
      request;
    let { encoder; response_body_buffer_size; create_push_stream; _ } = s in
    let active_stream = create_active_stream
        encoder
        response_body_buffer_size
        create_push_stream
    in
    (* From RFC7540§8.2:
         Promised requests [...] MUST NOT include a request body. *)
    let active_request = create_active_request active_stream request Body.empty
    in
    (* From RFC7540§8.2.1:
         Sending a PUSH_PROMISE frame creates a new stream and puts the stream
         into the "reserved (local)" state for the server and the "reserved
         (remote)" state for the client.

       Note: we do this before flushing the writer because request handlers might
       immediately call one of the `respond_with` functions and expect the stream
       to be in the `Reserved` state. *)
    promised_reqd.stream_state <- Reserved active_request;
    wakeup_writer ();
    promised_reqd
  | Error msg ->
    failwith msg

(* We can easily allow the priority of the PUSH request to be configurable.
 * We should allow users of this API to define the weight (maybe not strictly),
 * dependency on the current Reqd, and exclusivity *)
let unsafe_push t request =
  (* TODO: should we validate? *)
  (* From RFC7540§8.2:
       Promised requests MUST be cacheable (see [RFC7231], Section 4.2.3), MUST
       be safe (see [RFC7231], Section 4.2.1), and MUST NOT include a request
       body. *)
  match t.stream_state with
  | Idle
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders s) ->
    start_push_stream t s request
  | Open (ActiveRequest s)
  | HalfClosed s ->
    start_push_stream t s request
  (* Already checked in `push` *)
  | Reserved _
  | Closed _ -> assert false

let push t request =
  if fst t.error_code <> `Ok then
    failwith "http2af.Reqd.push: invalid state, currently handling error";
  if Stream_identifier.is_pushed t.id then
    (* From RFC7540§6.6:
         PUSH_PROMISE frames MUST only be sent on a peer-initiated stream that
         is in either the "open" or "half-closed (remote)" state. *)
    failwith "http2af.Reqd.push: PUSH_PROMISE frames MUST only be sent on a peer-initiated stream";
  unsafe_push t request

let finish_stream t reason =
  t.stream_state <- Closed { reason; ttl = initial_ttl };
  t.on_stream_closed ()

let reset_stream t error_code =
  let frame_info = Writer.make_frame_info t.id in
  Writer.write_rst_stream t.writer frame_info error_code;
  Writer.flush t.writer (fun () ->
    finish_stream t (ResetByUs error_code))

let close_stream t =
  match t.error_code with
  | _, Some error_code ->
    reset_stream t error_code
  | _, None ->
    match t.stream_state with
    | Open (FullHeaders _)
    | Open (ActiveRequest _) ->
      (* From RFC7540§8.1:
           A server can send a complete response prior to the client sending an
           entire request if the response does not depend on any portion of the
           request that has not been sent and received. When this is true, a
           server MAY request that the client abort transmission of a request
           without error by sending a RST_STREAM with an error code of NO_ERROR
           after sending a complete response (i.e., a frame with the END_STREAM
           flag). *)
      reset_stream t Error.NoError
    | HalfClosed _ ->
      Writer.flush t.writer (fun () -> finish_stream t Finished)
    | _ -> assert false

let _report_error ?request t s exn error_code =
  match s.response_state, fst t.error_code with
  | Waiting _, `Ok ->
    t.error_code <- (exn :> [`Ok | error]), Some error_code;
    let status =
      match (exn :> [error | Status.standard]) with
      | `Exn _                     -> `Internal_server_error
      | #Status.standard as status -> status
    in
    t.error_handler ?request exn (fun headers ->
      let response = Response.create ~headers status in
      unsafe_respond_with_streaming ~flush_headers_immediately:true t response)
  | Waiting _, `Exn _ ->
    (* XXX(seliopou): Decide what to do in this unlikely case. There is an
     * outstanding call to the [error_handler], but an intervening exception
     * has been reported as well. *)
    failwith "http2af.Reqd.report_exn: NYI"
  | Streaming(_response, response_body), `Ok ->
    Body.close_writer response_body;
    t.error_code <- (exn :> [`Ok | error]), Some error_code;
    reset_stream t error_code
  | Streaming(_response, response_body), `Exn _ ->
    Body.close_writer response_body;
    t.error_code <- fst t.error_code, Some error_code;
    reset_stream t error_code;
    Writer.close_and_drain t.writer
  | (Fixed _ | Complete _ | Streaming _ | Waiting _), _ ->
    (* XXX(seliopou): Once additional logging support is added, log the error
     * in case it is not spurious. *)
    (* Still need to send an RST_STREAM frame. Set t.error_code with
     * `error_code` and `flush_response_body` below will take care of it. *)
    t.error_code <- fst t.error_code, Some error_code;
    reset_stream t error_code

let report_error t exn error_code =
  match t.stream_state with
  | Idle
  | Reserved _
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders s) ->
    _report_error t s exn error_code
  | Open (ActiveRequest ({ request_state = { request; request_body; _ }; _ } as s))
  | HalfClosed ({ request_state = { request; request_body; _ }; _ } as s) ->
    Body.close_reader request_body;
    _report_error t s ~request exn error_code
  | Closed _ -> ()

let report_exn t exn =
  report_error t (`Exn exn) Error.InternalError

let try_with t f : (unit, exn) Result.result =
  try f (); Ok () with exn -> report_exn t exn; Error exn

(* Private API, not exposed to the user through http2af.mli *)

let close_request_body { request_body; _ } =
  Body.close_reader request_body

let error_code t =
  match fst t.error_code with
  | #error as error -> Some error
  | `Ok             -> None

let on_more_output_available t f =
  match t.stream_state with
  | Idle
  | Reserved _ -> assert false
  | Open (PartialHeaders _) -> assert false
  | Open (FullHeaders { response_state; _ })
  | Open (ActiveRequest { response_state; _ })
  | HalfClosed { response_state; _ } ->
    begin match response_state with
    | Waiting when_done_waiting ->
      (* Due to the flow-control window, this function might be called when
       * another callback is already stored. We don't enforce that the
       * currently stored callback is equal to `default_waiting` because it is
       * OK for that not to happen. *)
      when_done_waiting := f
    | Streaming(_, response_body) ->
      Body.when_ready_to_write response_body f
    | Fixed _ -> ()
    | Complete _ ->
      failwith "httpaf.Reqd.on_more_output_available: response already complete"
    end
  | Closed _ -> assert false

let requires_input t =
  match t.stream_state with
  | Idle
  | Reserved _
  | Open (PartialHeaders _)
  | Open (FullHeaders _) -> true
  | Open (ActiveRequest { request_state = { request_body; _}; _ }) ->
    not (Body.is_closed request_body)
  | HalfClosed _ -> false
  | Closed _ -> false

let response_body_requires_output response_body =
  not (Body.is_closed response_body)
  || Body.has_pending_output response_body

let requires_output t =
  match t.stream_state with
  | Idle
  | Reserved _ -> false
  (* From RFC7540§8.1:
       A server can send a complete response prior to the client sending an
       entire request if the response does not depend on any portion of the
       request that has not been sent and received. *)
  | Open (PartialHeaders _) -> false
  | Open (FullHeaders { response_state; _ })
  | Open (ActiveRequest { response_state; _ })
  | HalfClosed { response_state; _ } ->
    begin match response_state with
    | Complete _ -> false
    | Fixed { iovec = { len; _ }; _ } ->
      len > 0
    | Streaming (_, response_body) ->
      response_body_requires_output response_body
    | Waiting _ -> true
    end
  | Closed _ -> false

let is_complete t =
  not (requires_input t || requires_output t)

let flush_request_body t =
  let request_body = request_body t in
  if Body.has_pending_output request_body
  then try Body.execute_read request_body
  with exn -> report_exn t exn

let write_buffer_data writer ~off ~len frame_info buffer =
  match buffer with
  | `String str ->
    Writer.write_data writer ~off ~len frame_info str
  | `Bigstring bstr ->
    Writer.schedule_data writer ~off ~len frame_info bstr

let flush_response_body ~max_bytes t =
  match t.stream_state with
  | Open (FullHeaders { response_state; _ })
  | Open (ActiveRequest { response_state; _ })
  | HalfClosed { response_state; _ } ->
    begin match response_state with
    | Streaming (_, response_body) ->
      let written =
        Body.transfer_to_writer response_body
          t.writer
          ~max_frame_size:t.max_frame_size
          ~max_bytes t.id
      in
      if not (response_body_requires_output response_body) then
        close_stream t;
      written
    | Fixed r ->
      begin match r.iovec with
      | { buffer; off; len } as iovec ->
        if max_bytes < len then begin
          let frame_info =
            Writer.make_frame_info ~max_frame_size:t.max_frame_size t.id
          in
          write_buffer_data t.writer ~off ~len:max_bytes frame_info buffer;
          r.iovec <- Httpaf.IOVec.shift iovec max_bytes;
          max_bytes
        end else begin
          let frame_info =
            Writer.make_frame_info
              ~max_frame_size:t.max_frame_size
              ~flags:Flags.(set_end_stream default_flags)
              t.id
          in
          write_buffer_data t.writer ~off ~len frame_info buffer;
          close_stream t;
          len
        end
      end
    | Waiting _
    | Complete _ -> 0
    end
  | _ -> 0

let deliver_trailer_headers t headers =
  match t.stream_state with
  | Open (PartialHeaders _)
  | Open (FullHeaders _) -> assert false
  | Open (ActiveRequest s)
  | HalfClosed s ->
    (* TODO: call the schedule_trailers callback *)
    s.trailers <- Some headers
  | _ -> assert false

