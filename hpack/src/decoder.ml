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
open Angstrom

type t =
  { table : Dynamic_table.t
  ; max_capacity : int
  }

let create max_capacity =
  { table = Dynamic_table.create max_capacity; max_capacity }

let set_capacity { table; max_capacity } capacity =
  if capacity > max_capacity then
    (* From RFC7541§6.3:
     *   The new maximum size MUST be lower than or equal to the limit
     *   determined by the protocol using HPACK. A value that exceeds this
     *   limit MUST be treated as a decoding error. *)
    Error Decoding_error
  else (
    Dynamic_table.set_capacity table capacity;
    Ok ())

let[@inline] ok x = return (Ok x)

let[@inline] error x = return (Error x)

(* From RFC7541§5.1:
 *   decode I from the next N bits. *)
let decode_int prefix n =
  let max_prefix = (1 lsl n) - 1 in
  let i = prefix land max_prefix in
  if i < max_prefix then
    return i
  else
    let rec loop i m =
      any_uint8 >>= fun b ->
      let i = i + ((b land 127) lsl m) in
      if b land 0b1000_0000 == 0b1000_0000 then
        loop i (m + 7)
      else
        return i
    in
    loop i 0

let decode_string =
  any_uint8 >>= fun h ->
  (* From RFC7541§5.2:
   *   The number of octets used to encode the string literal, encoded as an
   *   integer with a 7-bit prefix (see Section 5.1). *)
  decode_int h 7 >>= fun string_length ->
  lift
    (fun string_data ->
      (* From RFC7541§5.2:
       *   A one-bit flag, H, indicating whether or not the octets of the
       *   string are Huffman encoded. *)
      if h land 0b1000_0000 == 0 then
        Ok string_data
      else
        Huffman.decode string_data)
    (take string_length)

let get_indexed_field table index =
  let static_table_size = Static_table.table_size in
  let dynamic_table_size = Dynamic_table.table_size table in
  (* From RFC7541§6.1:
   *   The index value of 0 is not used. It MUST be treated as a decoding
   *   error if found in an indexed header field representation. *)
  if
    index == 0
    || (* From RFC7541§2.3.3:
        *   Indices strictly greater than the sum of the lengths of both tables
        *   MUST be treated as a decoding error. *)
    index > static_table_size + dynamic_table_size
  then
    Error Decoding_error
  else if index <= static_table_size then
    (* From RFC7541§2.3.3:
     *   Indices between 1 and the length of the static table (inclusive) refer
     *   to elements in the static table (see Section 2.3.1). *)
    Ok Static_table.table.(index - 1)
  else
    (* From RFC7541§2.3.3:
     *   Indices strictly greater than the length of the static table refer to
     *   elements in the dynamic table (see Section 2.3.2). The length of the
     *   static table is subtracted to find the index into the dynamic
     *   table. *)
    Ok (Dynamic_table.get table (index - static_table_size - 1))

let decode_header_field table prefix prefix_length =
  decode_int prefix prefix_length >>= fun index ->
  lift2
    (fun name value ->
      match name, value with
      | Ok name, Ok value ->
        Ok (name, value)
      | Error e, _ | _, Error e ->
        Error e)
    (* From RFC7541§6.2.1:
     *   If the header field name matches the header field name of an entry
     *   stored in the static table or the dynamic table, the header field
     *   name can be represented using the index of that entry. In this case,
     *   [...] This value is always non-zero.
     *
     *   Otherwise, the header field name is represented as a string literal
     *   (see Section 5.2). A value 0 is used in place [...], followed by the
     *   header field name. *)
    (if index == 0 then
       decode_string
    else
      match get_indexed_field table index with
      | Ok (name, _) ->
        ok name
      | Error e ->
        error e)
    decode_string

let decode_headers ({ table; _ } as t) =
  let rec loop acc saw_first_header =
    at_end_of_input >>= fun is_eof ->
    if is_eof then
      ok acc
    else
      any_uint8 >>= fun b ->
      if b land 0b1000_0000 != 0 then
        (* From RFC7541§6.1: Indexed Header Field Representation
         *   An indexed header field starts with the '1' 1-bit pattern,
         *   followed by the index of the matching header field, represented as
         *   an integer with a 7-bit prefix (see Section 5.1). *)
        decode_int b 7 >>= fun index ->
        match get_indexed_field table index with
        | Ok (name, value) ->
          loop ({ name; value; sensitive = false } :: acc) true
        | Error e ->
          error e
      else if b land 0b1100_0000 == 0b0100_0000 then
        (* From RFC7541§6.2.1: Literal Header Field with Incremental Indexing
         *   A literal header field with incremental indexing representation
         *   starts with the '01' 2-bit pattern. In this case, the index of the
         *   entry is represented as an integer with a 6-bit prefix (see
         *   Section 5.1). *)
        decode_header_field table b 6 >>= function
        | Ok (name, value) ->
          (* From RFC7541§6.2.1: Literal Header Field with Incremental Indexing
           *   A literal header field with incremental indexing representation
           *   results in appending a header field to the decoded header list
           *   and inserting it as a new entry into the dynamic table. *)
          Dynamic_table.add table (name, value);
          loop ({ name; value; sensitive = false } :: acc) true
        | Error e ->
          error e
      else if b land 0b1111_0000 == 0 then
        (* From RFC7541§6.2.2: Literal Header Field without Indexing
         *   A literal header field without indexing representation starts with
         *   the '0000' 4-bit pattern. In this case, the index of the entry is
         *   represented as an integer with a 4-bit prefix (see Section
         *   5.1). *)
        decode_header_field table b 4 >>= function
        | Ok (name, value) ->
          loop ({ name; value; sensitive = false } :: acc) true
        | Error e ->
          error e
      else if b land 0b1111_0000 == 0b0001_0000 then
        (* From RFC7541§6.2.3: Literal Header Field Never Indexed
         *   A literal header field without indexing representation starts with
         *   the '0001' 4-bit pattern.
         *  The encoding of the representation is identical to the literal
         *  header field without indexing (see Section 6.2.2). *)
        decode_header_field table b 4 >>= function
        | Ok (name, value) ->
          loop ({ name; value; sensitive = true } :: acc) true
        | Error e ->
          error e
      else if b land 0b1110_0000 == 0b0010_0000 then
        if
          (* From RFC7541§6.3: Dynamic Table Size Update
           *   A dynamic table size update signals a change to the size of the
           *   dynamic table.
           *   A dynamic table size update starts with the '001' 3-bit
           *   pattern *)
          saw_first_header
        then
          (* From RFC7541§4.2: Maximum Table Size
           *   A change in the maximum size of the dynamic table is signaled
           *   via a dynamic table size update (see Section 6.3). This dynamic
           *   table size update MUST occur at the beginning of the first
           *   header block following the change to the dynamic table size. *)
          error Decoding_error
        else
          decode_int b 5 >>= fun capacity ->
          match set_capacity t capacity with
          | Ok () ->
            loop acc saw_first_header
          | Error e ->
            error e
      else
        error Decoding_error
  in
  loop [] false
