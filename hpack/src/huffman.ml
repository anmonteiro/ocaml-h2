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

open Types

let encoded_length s =
  let len = String.length s in
  let rec loop bits i =
    if i < len
    then
      let input = Char.code s.[i] in
      let _, len_in_bits = Huffman_table.encode_table.(input) in
      loop (bits + len_in_bits) (i + 1)
    else
      (* From RFC7541§5.2:
       *   As the Huffman-encoded data doesn't always end at an octet boundary,
       *   some padding is inserted after it, up to the next octet boundary. *)
      (bits + 7) / 8
  in
  loop 0 0

let encode t s =
  let bits = ref 0 in
  let bits_left = ref 40 in
  for i = 0 to String.length s - 1 do
    let code, code_len = Huffman_table.encode_table.(Char.code s.[i]) in
    bits_left := !bits_left - code_len;
    bits := !bits lor (code lsl !bits_left);
    while !bits_left <= 32 do
      Faraday.write_uint8 t (!bits lsr 32);
      bits := !bits lsl 8;
      bits_left := !bits_left + 8
    done
  done;
  if !bits_left < 40
  then (
    bits := !bits lor ((1 lsl !bits_left) - 1);
    (* add EOS and padding *)
    Faraday.write_uint8 t (!bits lsr 32))

let decode =
  let[@inline] add_output buffer c =
    if c <> '\000' then Buffer.add_char buffer c
  in
  let[@inline] exists_in_huffman_table token = token <> -1 in
  fun s ->
    let len = String.length s in
    let buffer = Buffer.create len in
    let rec loop id accept i =
      if i < len
      then (
        let input = Char.code s.[i] in
        let index = (id lsl 4) + (input lsr 4) in
        let id, _, output = Huffman_table.decode_table.(index) in
        add_output buffer output;
        if exists_in_huffman_table id
        then (
          let index = (id lsl 4) + (input land 0x0f) in
          let id, accept, output = Huffman_table.decode_table.(index) in
          add_output buffer output;
          if exists_in_huffman_table id
          then loop id accept (i + 1)
          else Error Decoding_error)
        else Error Decoding_error)
      else if not accept
      then Error Decoding_error
      else Ok ()
    in
    match loop 0 true 0 with
    | Ok _ -> Ok (Buffer.contents buffer)
    | Error e -> Error e
