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

module Writer = Serialize.Writer
module AB = Angstrom.Buffered

type error =
  [ `Malformed_response of string
  | `Exn of exn
  ]

type error_handler =
  error -> unit
type response_handler = Response.t -> [`read] Body.t  -> unit

type response_info =
  { response : Response.t
  ; response_body : [`read] Body.t
  }

type response_state =
  | Awaiting_response
  | PartialHeaders of Stream.partial_headers
  | FullHeaders
  | ActiveRequest of response_info

(* TODO: will probably need to handle half-closed (local) too, i.e. we're done
 * sending, but still waiting for a response. *)
type state =
  | Idle
    (* We purposely name this tag `HalfClosedLocal` to be explicit that once
     * a stream is in this state it means that the client has stopped sending
     * data. The server still has data to send back.
     *
     * Both `HalfClosedLocal` and `Open` states share the same payload type,
     * but we still preserve the distinct states to know whether the client has
     * finished sending the request body. *)
  | HalfClosedLocal of response_state
  | Open of response_state
    (* TODO: make the `request{,_exn}` / `response{,_exn}` functions work
     * in this state. *)
  | Closed of Stream.closed
    (* TODO: this is probably not right?! *)
  | Reserved of response_info

type t =
  { id                     : Stream_identifier.t
  ; request                : Request.t
  ; request_body           : [`read] Body.t
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

let create id request request_body ~max_frame_size writer error_handler response_handler on_stream_closed =
  { id
  ; request
  ; request_body
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
  t.request

let request_body t =
  t.request_body

let response t =
  match t.state with
  | Idle -> None
  | HalfClosedLocal response_info
  | Open response_info ->
    begin match response_info with
    | Awaiting_response
    | FullHeaders
    | PartialHeaders _ -> None
    | ActiveRequest { response; _ } ->
      Some response
    end
  | Reserved { response; _ } ->
    Some response
  | Closed _ -> None

let response_exn t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | HalfClosedLocal response_info
  | Open response_info ->
    begin match response_info with
    | Awaiting_response
    | FullHeaders
    | PartialHeaders _ ->
      failwith "h2.Respd.response_exn: response has not arrived"
    | ActiveRequest { response; _ } ->
        response
    end
  | Reserved { response; _ } ->
    response
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

let report_error t exn error_code =
  match t.state with
  | Idle
  | Reserved _
  | HalfClosedLocal Awaiting_response
  | HalfClosedLocal (PartialHeaders _)
  | Open Awaiting_response
  | Open (PartialHeaders _) -> assert false
  | HalfClosedLocal FullHeaders
  | Open FullHeaders ->
    begin match fst t.error_code with
    | `Ok ->
      (* TODO: flush the request body  *)
      Body.close_writer t.request_body;
      t.error_code <- (exn :> [`Ok | error]), Some error_code;
      t.error_handler exn;
      reset_stream t error_code
    | `Exn _
    | `Malformed_response _ ->
      (* XXX: Is this even possible? *)
      failwith "h2.Reqd.report_exn: NYI"
    end
  | HalfClosedLocal (ActiveRequest s)
  | Open (ActiveRequest s) ->
    begin match fst t.error_code with
    | `Ok ->
      Body.close_reader s.response_body;
      (* do we even need to execute this read? `close_reader` already does it. *)
      Body.execute_read s.response_body;
      (* TODO: flush the request body  *)
      Body.close_writer t.request_body;
      t.error_code <- (exn :> [`Ok | error]), Some error_code;
      t.error_handler exn;
      reset_stream t error_code
    | `Exn _
    | `Malformed_response _ ->
      (* XXX: Is this even possible? *)
      failwith "h2.Reqd.report_exn: NYI"
    end
  | Closed _ -> ()

let report_exn t exn =
  report_error t (`Exn exn) Error.InternalError

let try_with t f : (unit, exn) Result.result =
  try f (); Ok () with exn -> report_exn t exn; Error exn

(* Private API, not exposed to the user through h2.mli *)

let close_request_body { request_body; _ } =
  Body.close_reader request_body

let error_code t =
  match fst t.error_code with
  | #error as error -> Some error
  | `Ok             -> None

let on_more_output_available t f =
  if not (Body.is_closed t.request_body) then
    Body.when_ready_to_write t.request_body f

let request_body_requires_output response_body =
  not (Body.is_closed response_body)
  || Body.has_pending_output response_body

let requires_output t =
  match t.state with
    (* TODO: Right now  *)
  | Idle -> true
  | HalfClosedLocal _ -> false
    (* TODO: Does a reserved stream require output? *)
  | Reserved _ -> false
  | Open _ ->
    request_body_requires_output t.request_body
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
  | Open response_info ->
    let written =
      Body.transfer_to_writer t.request_body
        t.writer
        ~max_frame_size:t.max_frame_size
        ~max_bytes t.id
    in
    if not (request_body_requires_output t.request_body) then
      t.state <- HalfClosedLocal response_info;
    written
  | _ -> 0

