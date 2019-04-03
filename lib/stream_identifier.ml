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

(* From RFC7540§5.1.1:
 *   Streams are identified with an unsigned 31-bit integer. *)
type t = int32

let ( === ) = Int32.equal

let[@inline] ( <= ) s1 s2 = Int32.compare s1 s2 <= 0

let[@inline] ( > ) s1 s2 = Int32.compare s1 s2 > 0

let[@inline] ( >= ) s1 s2 = Int32.compare s1 s2 >= 0

(* From RFC7540§5.1.1:
 *   A stream identifier of zero (0x0) is used for connection control messages;
 *   the stream identifier of zero cannot be used to establish a new stream. *)
let connection = Int32.zero

(* From RFC7540§5.1.1:
 *   A stream identifier of zero (0x0) is used for connection control messages;
 *   the stream identifier of zero cannot be used to establish a new stream. *)
let[@inline] is_connection id = Int32.equal id connection

(* From RFC7540§5.1.1:
 *   Streams initiated by a client MUST use odd-numbered stream
 *   identifiers [...]. *)
let[@inline] is_request id = Int32.rem id 2l === 1l

(* From RFC7540§5.1.1:
 *   Streams initiated by [...] the server MUST use even-numbered stream
 *   identifiers. A stream identifier of zero (0x0) is used for connection
 *   control messages [...]. *)
let[@inline] is_pushed = function 0l -> false | n -> Int32.rem n 2l === 0l

(* From RFC7540§5.1.1:
 *   Streams are identified with an unsigned 31-bit integer. *)
let max_stream_id = Int32.max_int
