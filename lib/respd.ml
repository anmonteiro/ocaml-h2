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
open Stream

type error =
  [ `Malformed_response of string
  | `Invalid_response_body_length of Response.t
  | `Exn of exn
  ]

type error_handler = error -> unit

type response_handler = Response.t -> [ `read ] Body.t -> unit

type response_info =
  { response : Response.t
  ; response_body : [ `read ] Body.t
  ; mutable response_body_bytes : int64
        (* We're not doing anything with these yet, we could probably have a
         * `Reqd.schedule_read_trailers` function that would be called once
         * trailer headers are emitted. *)
  ; mutable trailers_parser : Stream.partial_headers option
  ; mutable trailers : Headers.t option
  }

type active_request =
  { request : Request.t
  ; request_body : [ `read ] Body.t
  ; response_handler : response_handler
  }

type active_state =
  (response_info, response_info remote_state) Stream.active_state

type state = (active_state, active_request, active_request) Stream.state

type t = (state, [ `Ok | error ], error_handler) Stream.stream

let create_active_response response response_body =
  ActiveMessage
    { response
    ; response_body
    ; response_body_bytes = Int64.zero
    ; trailers_parser = None
    ; trailers = None
    }

let request t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.request: request is not active"
  | Reserved { request; _ } ->
    request
  | Active (_, { request; _ }) ->
    request
  | Closed _ ->
    failwith "h2.Respd.request: request has ended"

let request_body t =
  match t.state with
  | Idle ->
    failwith "h2.Respd.request: request is not active"
  | Reserved { request_body; _ } ->
    request_body
  | Active (_, { request_body; _ }) ->
    request_body
  | Closed _ ->
    failwith "h2.Respd.request: request has ended"

let response t =
  match t.state with
  | Idle ->
    None
  | Reserved _ ->
    None
  | Active
      ( ( Open (ActiveMessage { response; _ })
        | HalfClosed (ActiveMessage { response; _ }) )
      , _ ) ->
    Some response
  | Active ((Open _ | HalfClosed _), _) ->
    None
  | Closed _ ->
    None

let response_exn t =
  match t.state with
  | Idle | Reserved _ ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Active
      ( ( Open (ActiveMessage { response; _ })
        | HalfClosed (ActiveMessage { response; _ }) )
      , _ ) ->
    response
  | Active ((Open _ | HalfClosed _), _) ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Closed _ ->
    assert false

let response_body_exn t =
  match t.state with
  | Idle | Reserved _ ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Active
      ( ( Open (ActiveMessage { response_body; _ })
        | HalfClosed (ActiveMessage { response_body; _ }) )
      , _ ) ->
    response_body
  | Active ((Open _ | HalfClosed _), _) ->
    failwith "h2.Respd.response_exn: response has not arrived"
  | Closed _ ->
    assert false

(* let close_stream t =
 *match t.error_code with
 *| _, Some error_code ->
 *  reset_stream t error_code
 *| _, None ->
 *  (match t.state with
 *  | HalfClosedLocal (ActiveMessage _) ->
 *    (* From RFC7540§8.1: A server can send a complete response prior to the
 *       client sending an entire request if the response does not depend on
 *       any portion of the request that has not been sent and received. When
 *       this is true, a server MAY request that the client abort transmission
 *       of a request without error by sending a RST_STREAM with an error code
 *       of NO_ERROR after sending a complete response (i.e., a frame with the
 *       END_STREAM flag). *)
 *    reset_stream t Error.NoError
 *  | Open (ActiveMessage _) ->
 *    Writer.flush t.writer (fun () -> finish_stream t Finished)
 *  | _ ->
 *    assert false) *)

let close_stream t =
  match t.state with
  | Active (HalfClosed _, _) ->
    (* easy case, just transition to the closed state. *)
    finish_stream t Finished
  | Active (Open _, _) ->
    (* Still not done sending, reset stream with no error? *)
    (* TODO: *)
    ()
  | _ ->
    ()

let _report_error t s ?response_body exn error_code =
  match fst t.error_code with
  | `Ok ->
    (match response_body with
    | Some response_body ->
      Body.close_reader response_body;
      (* do we even need to execute this read? `close_reader` already does it. *)
      Body.execute_read response_body
    | None ->
      ());
    (* TODO: flush the request body *)
    Body.close_writer s.request_body;
    t.error_code <- (exn :> [ `Ok | error ]), Some error_code;
    t.error_handler exn;
    reset_stream t error_code
  | `Exn _ | `Invalid_response_body_length _ | `Malformed_response _ ->
    (* XXX: Is this even possible? *)
    failwith "h2.Reqd.report_exn: NYI"

let report_error t exn error_code =
  match t.state with
  | Idle
  | Reserved _
  | Active
      ( ( Open (WaitingForPeer | PartialHeaders _)
        | HalfClosed (WaitingForPeer | PartialHeaders _) )
      , _ ) ->
    assert false
  | Active ((Open FullHeaders | HalfClosed FullHeaders), s) ->
    _report_error t s exn error_code
  | Active
      ( ( Open (ActiveMessage { response_body; _ })
        | HalfClosed (ActiveMessage { response_body; _ }) )
      , s ) ->
    _report_error t s ~response_body exn error_code
  | Closed _ ->
    ()

let close_request_body { request_body; _ } = Body.close_reader request_body

let error_code t =
  match fst t.error_code with #error as error -> Some error | `Ok -> None

let on_more_output_available t f =
  match t.state with
  | Idle | Reserved _ ->
    assert false
  | Active (_, { request_body; _ }) ->
    if not (Body.is_closed request_body) then
      Body.when_ready_to_write request_body f
  | Closed _ ->
    assert false

let request_body_requires_output response_body =
  (not (Body.is_closed response_body)) || Body.has_pending_output response_body

let requires_output t =
  match t.state with
  (* TODO: Right now *)
  | Idle ->
    true
  (* TODO: Does a reserved stream require output? *)
  | Reserved _ ->
    false
  | Active (Open _, { request_body; _ }) ->
    request_body_requires_output request_body
  | Active (HalfClosed _, _) ->
    false
  | Closed _ ->
    false

let write_buffer_data writer ~off ~len frame_info buffer =
  match buffer with
  | `String str ->
    Writer.write_data writer ~off ~len frame_info str
  | `Bigstring bstr ->
    Writer.schedule_data writer ~off ~len frame_info bstr

let flush_request_body t ~max_bytes =
  match t.state with
  | Active (Open active_state, ({ request_body; _ } as s)) ->
    let written =
      Body.transfer_to_writer
        request_body
        t.writer
        ~max_frame_size:t.max_frame_size
        ~max_bytes
        t.id
    in
    if not (request_body_requires_output request_body) then
      t.state <- Active (HalfClosed active_state, s);
    written
  | _ ->
    0

let deliver_trailer_headers t headers =
  match t.state with
  | Active ((Open (ActiveMessage s) | HalfClosed (ActiveMessage s)), _) ->
    (* TODO: call the schedule_trailers callback *)
    s.trailers <- Some headers
  | _ ->
    assert false
