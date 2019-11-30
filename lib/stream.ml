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

module AB = Angstrom.Buffered
module Writer = Serialize.Writer

type partial_headers =
  { mutable parse_state : (Headers.t, Hpack.error) result AB.state
  ; end_stream : bool
  }

type 'active_peer remote_state =
  (* A stream is in this state when it's waiting for the peer to initiate a
   * response. In practice, it only matters for the client implementation, when
   * a client has opened a stream but is still waiting on the server to send
   * the first bytes of the response. *)
  | WaitingForPeer
  (* A PartialHeaders state is entered when the endpoint sees the first HEADERS
   * frame from the peer for a given stream. Its payload is an
   * Angstrom.Buffered parse state. *)
  | PartialHeaders of partial_headers
  (* A stream transitions from the PartialHeaders state to the FullHeaders
   * state when the endpoint has finished parsing all the bytes in a group of
   * HEADER / CONTINUATION frames that the peer has sent.
   * This state doesn't carry any payload because the stream will immediately
   * transition to the ActiveMessage state once the message has been validated
   * according to RFC7540§8.1.2. *)
  | FullHeaders
  (* The ActiveMessage state carries information about the current remote
   * message being processed by the endpoint. *)
  | ActiveMessage of 'active_peer

type closed_reason =
  | Finished
  (* TODO: we could abide by the following by either 1) having I/O runtime
   * support for timers or 2) by simply counting the number of frames received
   * after we've sent an RST_STREAM?
   *
   * From RFC7540§5.4.2:
   *   Normally, an endpoint SHOULD NOT send more than one RST_STREAM frame for
   *   any stream. However, an endpoint MAY send additional RST_STREAM frames
   *   if it receives frames on a closed stream after more than a round-trip
   *   time. This behavior is permitted to deal with misbehaving
   *   implementations. *)
  | ResetByUs of Error_code.t
  (* Received an RST_STREAM frame from the peer. *)
  | ResetByThem of Error_code.t

type closed =
  { reason : closed_reason
        (* When a stream is closed, we may want to keep it around in the hash
         * table for a while (e.g. to know whether this stream was reset by the
         * peer - some error handling code depends on that). We start with a
         * default value, and on every writer yield we decrement it. If it
         * reaches 0, the stream is finally removed from the hash table. *)
  ; mutable ttl : int
  }

type ('opn, 'half_closed) active_state =
  | Open of 'opn remote_state
  | HalfClosed of 'half_closed

type ('active_state, 'active, 'reserved) state =
  | Idle
  | Reserved of 'reserved
  | Active of 'active_state * 'active
  | Closed of closed
  constraint 'active_state = (_, _) active_state

type ('state, 'error_code, 'error_handler) stream =
  { id : Stream_identifier.t
  ; writer : Serialize.Writer.t
  ; error_handler : 'error_handler
  ; mutable error_code : 'error_code * Error_code.t option
  ; mutable state : 'state
        (* The largest frame payload we're allowed to write. *)
  ; mutable max_frame_size : int
  ; on_close_stream : active:bool -> closed -> unit
  }
  constraint 'state = (_, _, _) state

let initial_ttl = 10

let create id ~max_frame_size writer error_handler on_close_stream =
  { id
  ; writer
  ; error_handler
    (* From RFC7540§5.1:
     *   idle: All streams start in the "idle" state. *)
  ; state = Idle
  ; error_code = `Ok, None
  ; max_frame_size
  ; on_close_stream
  }

let id { id; _ } = id

let is_idle t = match t.state with Idle -> true | _ -> false

let is_open t = match t.state with Active (Open _, _) -> true | _ -> false

let finish_stream t reason =
  let active = match t.state with Active _ -> true | _ -> false in
  let closed = { reason; ttl = initial_ttl } in
  t.on_close_stream ~active closed;
  t.state <- Closed closed

let reset_stream t error_code =
  let frame_info = Writer.make_frame_info t.id in
  Writer.write_rst_stream t.writer frame_info error_code;
  Writer.flush t.writer (fun () -> finish_stream t (ResetByUs error_code))
