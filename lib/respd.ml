(*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

(* TODO(anmonteiro): think about whether we wanna expose this module. it might
 * be helpful to expose a way to reset streams, and I think we'd need a
 * reference to the Respd *)

module Writer = Serialize.Writer
module AB = Angstrom.Buffered

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Exn of exn
  ]

type error_handler = error -> unit
type response_handler = Response.t -> [`read] Body.t  -> unit

type response_info =
  { response : Response.t
  ; response_body : [`read] Body.t
  ; mutable response_body_bytes : int64
    (* We're not doing anything with these yet, we could probably have a
     * `Reqd.schedule_read_trailers` function that would be called once trailer
     * headers are emitted. *)
  ; mutable trailers_parser      : Stream.partial_headers option
  ; mutable trailers             : Headers.t option
  }

(* TODO: document, esp. that FullHeaders is only for error handling *)
type response_state =
  | Awaiting_response
  | PartialHeaders of Stream.partial_headers
  | FullHeaders
  | ActiveResponse of response_info

type pending_response =
    (* We purposely name this tag `HalfClosedLocal` to be explicit that once
     * a stream is in this state it means that the client has stopped sending
     * data. The server still has data to send back. *)
  | HalfClosedLocal
  | Open

type active_request =
  { mutable response_state : response_state
  ; mutable stream_state   : pending_response
  ; request                : Request.t
  ; request_body           : [`read] Body.t
  }

type state =
  | Idle
  | Active of active_request
    (* TODO: make the `request{,_exn}` / `response{,_exn}` functions work
     * in this state. *)
  | Closed of Stream.closed
  | Reserved of active_request

type t =
  { id                     : Stream_identifier.t
  ; writer                 : Writer.t
  ; error_handler          : error_handler
  ; response_handler       : response_handler
  ; mutable error_code     : [`Ok | error ] * Error.error_code option
  ; mutable state   : state
    (* The largest frame payload we're allowed to write. *)
  ; mutable max_frame_size : int
  ; on_stream_closed       : unit -> unit
  }

let default_waiting = Sys.opaque_identity (fun () -> ())
let initial_ttl = 10

let create_active_response response response_body =
  ActiveResponse
    { response
    ; response_body
    ; response_body_bytes = Int64.zero
    ; trailers_parser = None
    ; trailers = None
    }

let create id ~max_frame_size writer error_handler response_handler on_stream_closed =
  { id
  ; writer
  ; error_handler
  ; response_handler
  ; state   = Idle
  ; error_code     = `Ok, None
  ; max_frame_size
  ; on_stream_closed
  }

let done_waiting when_done_waiting =
  let f = !when_done_waiting in
  when_done_waiting := default_waiting;
  f ()

let request t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.request: request is not active"
  | Reserved { request; _ } -> request
  | Active { request; _ } -> request
  | Closed _ ->
    failwith "h2.Respd.request: request has ended"

let request_body t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.request: request is not active"
  | Reserved { request_body; _ } -> request_body
  | Active { request_body; _ } -> request_body
  | Closed _ ->
    failwith "h2.Respd.request: request has ended"

let response t =
  match t.state with
  | Idle -> None
  | Reserved { response_state; _ }
  | Active { response_state; _ } ->
    begin match response_state with
    | Awaiting_response
    | FullHeaders
    | PartialHeaders _ -> None
    | ActiveResponse { response; _ } ->
      Some response
    end
  | Closed _ -> None

let response_exn t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Reserved { response_state; _ }
  | Active { response_state; _ } ->
    begin match response_state with
    | Awaiting_response
    | FullHeaders
    | PartialHeaders _ ->
      failwith "h2.Respd.response_exn: response has not arrived"
    | ActiveResponse { response; _ } ->
      response
    end
  | Closed _ -> assert false

let response_body_exn t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Reserved { response_state; _ }
  | Active { response_state; _ } ->
    begin match response_state with
    | Awaiting_response
    | FullHeaders
    | PartialHeaders _ ->
      failwith "h2.Respd.response_exn: response has not arrived"
    | ActiveResponse { response_body; _ } ->
      response_body
    end
  | Closed _ -> assert false

let finish_stream t reason =
  t.state <- Closed { reason; ttl = initial_ttl };
  t.on_stream_closed ()

let reset_stream t error_code =
  let frame_info = Writer.make_frame_info t.id in
  Writer.write_rst_stream t.writer frame_info error_code;
  Writer.flush t.writer (fun () ->
    finish_stream t (ResetByUs error_code))

(* let close_stream t =
  match t.error_code with
  | _, Some error_code ->
    reset_stream t error_code
  | _, None ->
    match t.state with
    | HalfClosedLocal (ActiveRequest _) ->
      (* From RFC7540§8.1:
           A server can send a complete response prior to the client sending an
           entire request if the response does not depend on any portion of the
           request that has not been sent and received. When this is true, a
           server MAY request that the client abort transmission of a request
           without error by sending a RST_STREAM with an error code of NO_ERROR
           after sending a complete response (i.e., a frame with the END_STREAM
           flag). *)
      reset_stream t Error.NoError
    | Open (ActiveRequest  _) ->
      Writer.flush t.writer (fun () -> finish_stream t Finished)
    | _ -> assert false *)

let close_stream t =
  match t.state with
  | Active { stream_state = HalfClosedLocal; _ } ->
    (* easy case, just transition to the closed state. *)
    finish_stream t Finished
  | Active { stream_state = Open; _ } ->
    (* Still not done sending, reset stream with no error? *)
    (* TODO: *)
    ()
  | _ -> ()

let _report_error t s ?response_body exn error_code =
  match fst t.error_code with
  | `Ok ->
    begin match response_body with
    | Some response_body ->
      Body.close_reader response_body;
      (* do we even need to execute this read? `close_reader` already does it. *)
      Body.execute_read response_body;
    | None -> ()
    end;
    (* TODO: flush the request body  *)
    Body.close_writer s.request_body;
    t.error_code <- (exn :> [`Ok | error]), Some error_code;
    t.error_handler exn;
    reset_stream t error_code
  | `Exn _
  | `Invalid_response_body_length _
  | `Malformed_response _ ->
    (* XXX: Is this even possible? *)
    failwith "h2.Reqd.report_exn: NYI"

let report_error t exn error_code =
  match t.state with
  | Idle
  | Reserved { response_state = Awaiting_response | PartialHeaders _ | ActiveResponse _; _ }
  | Active { response_state = Awaiting_response | PartialHeaders _; _ } ->
    assert false
  | Reserved ({ response_state = FullHeaders; _ } as s)
  | Active ({ response_state = FullHeaders; _ } as s) ->
    _report_error t s exn error_code
  | Active ({ response_state = ActiveResponse { response_body; _ }; _ } as s) ->
    _report_error t s ~response_body exn error_code
  | Closed _ -> ()

let close_request_body { request_body; _ } =
  Body.close_reader request_body

let error_code t =
  match fst t.error_code with
  | #error as error -> Some error
  | `Ok             -> None

let on_more_output_available t f =
  match t.state with
  | Idle
  | Reserved _ -> assert false
  | Active { request_body; _ } ->
    if not (Body.is_closed request_body) then
      Body.when_ready_to_write request_body f
  | Closed _ -> assert false

let request_body_requires_output response_body =
  not (Body.is_closed response_body)
  || Body.has_pending_output response_body

let requires_output t =
  match t.state with
    (* TODO: Right now  *)
  | Idle -> true
  | Active { stream_state = HalfClosedLocal; _ } -> false
    (* TODO: Does a reserved stream require output? *)
  | Reserved _ -> false
  | Active { stream_state = Open; request_body; _ } ->
    request_body_requires_output request_body
  | Closed _ -> false

let write_buffer_data writer ~off ~len frame_info buffer =
  match buffer with
  | `String str ->
    Writer.write_data writer ~off ~len frame_info str
  | `Bigstring bstr ->
    Writer.schedule_data writer ~off ~len frame_info bstr

(* let flush_request_body t s =
  if Body.has_pending_output s.request_body then
    Body.transfer_to_writer s.request_body t.writer
      ~max_frame_size:t.max_frame_size *)

let flush_request_body t ~max_bytes =
  match t.state with
  | Active ({ stream_state = Open; request_body; _ } as s) ->
    let written =
      Body.transfer_to_writer request_body
        t.writer
        ~max_frame_size:t.max_frame_size
        ~max_bytes t.id
    in
    if not (request_body_requires_output request_body) then
      s.stream_state <- HalfClosedLocal;
    written
  | _ -> 0

let deliver_trailer_headers t headers =
  match t.state with
  | Active { response_state = ActiveResponse s; _ } ->
    (* TODO: call the schedule_trailers callback *)
    s.trailers <- Some headers
  | _ -> assert false

