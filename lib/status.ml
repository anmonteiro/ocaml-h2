(*----------------------------------------------------------------------------
 *  Copyright (c) 2017 Inhabited Type LLC.
 *  Copyright (c) 2019 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

(* From RFC7540§8.1.1:
 *   HTTP/2 removes support for the 101 (Switching Protocols) informational
 *   status code ([RFC7231], Section 6.2.2).
 *
 *   Note: While the above is true, we don't enforce in this library, as it
 *   makes unifying types with http/af much easier. `H2.Status.t` is, thus, a
 *   strict superset of `Httpaf.Status.t`. *)

include (
  Httpaf.Status :
    module type of Httpaf.Status
      with type client_error := Httpaf.Status.client_error
       and type standard := Httpaf.Status.standard
       and type t := Httpaf.Status.t)

type client_error =
  [ Httpaf.Status.client_error
  | (* From RFC7540§9.1.2:
     *   The 421 (Misdirected Request) status code indicates that the request
     *   was directed at a server that is not able to produce a response. This
     *   can be sent by a server that is not configured to produce responses
     *   for the combination of scheme and authority that are included in the
     *   request URI. *)
    `Misdirected_request
  ]

type standard =
  [ Httpaf.Status.standard
  | client_error
  ]

type t =
  [ standard
  | `Code of int
  ]

(* Note: The value for reason phrases is never actually serialized to the
 * input or output channels.
 *
 * From RFC7540§8.1.2.4:
 *   HTTP/2 does not define a way to carry the version or reason phrase that is
 *   included in an HTTP/1.1 status line. *)
let default_reason_phrase = function
  | `Misdirected_request ->
    "Misdirected Request"
  | #Httpaf.Status.standard as t ->
    Httpaf.Status.default_reason_phrase t

let to_code = function
  | `Misdirected_request ->
    421
  | #Httpaf.Status.t as t ->
    Httpaf.Status.to_code t

let unsafe_of_code = function
  | 421 ->
    `Misdirected_request
  | c ->
    (Httpaf.Status.unsafe_of_code c :> t)

let of_code = function
  | 421 ->
    `Misdirected_request
  | c ->
    (Httpaf.Status.of_code c :> t)

let is_informational = function
  | `Misdirected_request ->
    false
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_informational t

let is_successful = function
  | `Misdirected_request ->
    false
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_successful t

let is_redirection = function
  | `Misdirected_request ->
    false
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_redirection t

let is_client_error = function
  | `Misdirected_request ->
    true
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_client_error t

let is_server_error = function
  | `Misdirected_request ->
    false
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_server_error t

let is_error = function
  | `Misdirected_request ->
    true
  | #Httpaf.Status.t as t ->
    Httpaf.Status.is_error t

let to_string = function
  | `Misdirected_request ->
    "421"
  | #Httpaf.Status.t as t ->
    Httpaf.Status.to_string t

let of_string x = of_code (int_of_string x)

let pp_hum fmt t = Format.fprintf fmt "%u" (to_code t)
