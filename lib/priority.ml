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

type t =
  { exclusive : bool
  ; stream_dependency : Stream_identifier.t
  ; weight : int
  }

(* From RFC7540§5.3.5:
 *   All streams are initially assigned a non-exclusive dependency on stream
 *   0x0. Pushed streams (Section 8.2) initially depend on their associated
 *   stream. In both cases, streams are assigned a default weight of 16. *)
let default_priority =
  { exclusive = false; stream_dependency = 0l; weight = 16 }

(* From RFC7540§5.4.1:
 *   All dependent streams are allocated an integer weight between 1 and 256
 *   (inclusive). *)
let highest_priority =
  { exclusive = false; stream_dependency = 0l; weight = 256 }

(* --- Exclusive flag ---
 *
 * From RFC7540§5.4.1:
 *   +-+-------------------------------------------------------------+
 *   |E|                  Stream Dependency (31)                     |
 *   +-+-------------+-----------------------------------------------+
 *   |   Weight (8)  |
 *   +-+-------------+
 *)

let test_exclusive n = test_bit_int32 n 31
let set_exclusive n = set_bit_int32 n 31
let clear_exclusive n = clear_bit_int32 n 31

let equal p1 p2 =
  p1.weight = p2.weight
  && Int32.equal p1.stream_dependency p2.stream_dependency
  && p1.exclusive = p2.exclusive
