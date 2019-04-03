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

open Util

type t = int

(* From RFC7540§6.2:
 *   Flags that have no defined semantics for a particular frame type MUST be
 *   ignored and MUST be left unset (0x0) when sending. *)
let default_flags = 0x0

(* From RFC7540§6.2:
 *   END_STREAM (0x1): When set, bit 0 indicates that the header block (Section
 *   4.3) is the last that the endpoint will send for the identified stream. *)
let test_end_stream x = test_bit x 0

let set_end_stream x = set_bit x 0

let clear_end_stream x = clear_bit x 0

(* From RFC7540§6.7:
 *   ACK (0x1): When set, bit 0 indicates that this PING frame is a PING
 *   response. *)
let test_ack x = test_bit x 0

let set_ack x = set_bit x 0

(* From RFC7540§6.2:
 *   END_HEADERS (0x4): When set, bit 2 indicates that this frame contains an
 *   entire header block (Section 4.3) and is not followed by any CONTINUATION
 *   frames. *)
let test_end_header x = test_bit x 2

let set_end_header x = set_bit x 2

(* From RFC7540§6.2:
 *   PADDED (0x8): When set, bit 3 indicates that the Pad Length field and any
 *   padding that it describes are present. *)
let test_padded x = test_bit x 3

let set_padded x = set_bit x 3

(* From RFC7540§6.2:
 *   PRIORITY (0x20): When set, bit 5 indicates that the Exclusive Flag (E),
 *   Stream Dependency, and Weight fields are present; see Section 5.3. *)
let test_priority x = test_bit x 5

let set_priority x = set_bit x 5
