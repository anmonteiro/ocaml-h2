(*----------------------------------------------------------------------------
    Copyright (c) 2019 António Nuno Monteiro

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from this
    software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

module AB = Angstrom.Buffered

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

type closed =
  { reason : closed_reason
    (* When a stream is closed, we may want to keep it around in the hash
     * table for a while (e.g. to know whether this stream was reset by the
     * peer - some error handling code depends on that). We start with a
     * default value, and on every writer yield we decrement it. If it
     * reaches 0, the stream is finally removed from the hash table. *)
  ; mutable ttl : int
  }
