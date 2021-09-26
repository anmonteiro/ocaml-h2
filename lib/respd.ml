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
  | `Protocol_error of Error_code.t * string
  | `Exn of exn
  ]

type error_handler = error -> unit

type response_handler = Response.t -> [ `read ] Body.t -> unit

type response_info =
  { response : Response.t
  ; response_body : [ `read ] Body.t
  ; mutable response_body_bytes : int64
  ; mutable trailers_parser : Stream.partial_headers option
  }

type trailers_handler = Headers.t -> unit

type active_request =
  { request : Request.t
  ; request_body : [ `writer ] Body.t
  ; response_handler : response_handler
  ; trailers_handler : trailers_handler
  }

type active_state =
  (response_info, response_info remote_state) Stream.active_state

type state =
  ( active_state
  , active_request
  , active_request Stream.remote_state )
  Stream.state

type t = (state, [ `Ok | error ], error_handler) Stream.stream

let create_active_response response response_body =
  ActiveMessage
    { response
    ; response_body
    ; response_body_bytes = Int64.zero
    ; trailers_parser = None
    }

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
    failwith "h2.Respd.response_exn: stream already closed"

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
  (* TODO: reserved *)
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

(* returns whether we should send an RST_STREAM frame or not. *)
let _report_error t ?response_body error error_code =
  match fst t.error_code with
  | `Ok ->
    (match response_body with
    | Some response_body ->
      Body.close_reader response_body;
      (* do we even need to execute this read? `close_reader` already does
         it. *)
      Body.execute_read response_body
    | None ->
      ());
    t.error_code <- (error :> [ `Ok | error ]), Some error_code;
    t.error_handler error;
    true
  | `Exn _
  | `Protocol_error _
  | `Invalid_response_body_length _
  | `Malformed_response _ ->
    (* XXX: Is this even possible? *)
    failwith "h2.Reqd.report_exn: NYI"

let report_error t error error_code =
  match t.state with
  | Active
      ( ( Open (ActiveMessage { response_body; _ })
        | HalfClosed (ActiveMessage { response_body; _ }) )
      , s ) ->
    Body.close_writer s.request_body;
    if _report_error t ~response_body error error_code then
      reset_stream t error_code
  | Reserved (ActiveMessage s) | Active (_, s) ->
    Body.close_writer s.request_body;
    if _report_error t error error_code then
      reset_stream t error_code
  | Reserved _ ->
    (* Streams in the reserved state don't yet have a stream-level error
     * handler registered with them *)
    ()
  | Idle | Closed _ ->
    (* Not allowed to send RST_STREAM frames in these states *)
    ignore (_report_error t error error_code)

let error_code t =
  match fst t.error_code with #error as error -> Some error | `Ok -> None

let requires_output t =
  match t.state with
  | Idle ->
    true
  | Reserved _ ->
    false
  | Active (Open _, _) ->
    true
  | Active (HalfClosed _, _) ->
    false
  | Closed _ ->
    false

let flush_request_body t ~max_bytes =
  match t.state with
  | Active (Open active_state, ({ request_body; _ } as s)) ->
    if Body.has_pending_output request_body && max_bytes > 0 then
      Body.transfer_to_writer
        request_body
        t.writer
        ~max_frame_size:t.max_frame_size
        ~max_bytes
        t.id
    else if Body.is_closed request_body then (
      (* closed and no pending output *)
      (* From RFC7540§6.9.1:
       *   Frames with zero length with the END_STREAM flag set (that is, an
       *   empty DATA frame) MAY be sent if there is no available space in
       *   either flow-control window. *)
      let frame_info =
        Writer.make_frame_info
          ~max_frame_size:t.max_frame_size
          ~flags:Flags.(set_end_stream default_flags)
          t.id
      in
      Writer.schedule_data t.writer frame_info ~len:0 Bigstringaf.empty;
      t.state <- Active (HalfClosed active_state, s);
      0)
    else (* not closed and no pending output *)
      0
  | _ ->
    0

let deliver_trailer_headers t headers =
  match t.state with
  | Active
      ( (Open (ActiveMessage _) | HalfClosed (ActiveMessage _))
      , { trailers_handler; _ } ) ->
    trailers_handler headers
  | _ ->
    assert false

let flush_response_body t =
  match t.state with
  | Active
      ( ( Open (ActiveMessage { response_body; _ })
        | HalfClosed (ActiveMessage { response_body; _ }) )
      , _ ) ->
    if Body.has_pending_output response_body then (
      try Body.execute_read response_body with
      | exn ->
        report_error t (`Exn exn) InternalError)
  | _ ->
    ()
