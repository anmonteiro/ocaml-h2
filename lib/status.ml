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

(* If we ever drop support for OCaml < 4.06 we can use a variant of the
 * following, and remove all the function forwarding.
 *
 * From https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec249:
 *
 *   Prior to OCaml 4.06, there were a number of restrictions: one could only
 *   remove types and modules at the outermost level (not inside submodules),
 *   and in the case of with type the definition had to be another type
 *   constructor with the same type parameters. *)
(* include (Httpaf.Status : Status with type informational := [ | `Continue ]) *)

(* From RFC7540§8.1.1:
 *   HTTP/2 removes support for the 101 (Switching Protocols) informational
 *   status code ([RFC7231], Section 6.2.2). *)
type informational = [ `Continue ]

type successful = Httpaf.Status.successful

type redirection = Httpaf.Status.redirection

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

type server_error = Httpaf.Status.server_error

type standard =
  [ informational
  | successful
  | redirection
  | client_error
  | server_error
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

let really_unsafe_of_code = function
  (* Informational *)
  | 100 ->
    `Continue
  (* Successful *)
  | 200 ->
    `OK
  | 201 ->
    `Created
  | 202 ->
    `Accepted
  | 203 ->
    `Non_authoritative_information
  | 204 ->
    `No_content
  | 205 ->
    `Reset_content
  | 206 ->
    `Partial_content
  (* Redirection *)
  | 300 ->
    `Multiple_choices
  | 301 ->
    `Moved_permanently
  | 302 ->
    `Found
  | 303 ->
    `See_other
  | 304 ->
    `Not_modified
  | 305 ->
    `Use_proxy
  | 307 ->
    `Temporary_redirect
  (* Client error *)
  | 400 ->
    `Bad_request
  | 401 ->
    `Unauthorized
  | 402 ->
    `Payment_required
  | 403 ->
    `Forbidden
  | 404 ->
    `Not_found
  | 405 ->
    `Method_not_allowed
  | 406 ->
    `Not_acceptable
  | 407 ->
    `Proxy_authentication_required
  | 408 ->
    `Request_timeout
  | 409 ->
    `Conflict
  | 410 ->
    `Gone
  | 411 ->
    `Length_required
  | 412 ->
    `Precondition_failed
  | 413 ->
    `Payload_too_large
  | 414 ->
    `Uri_too_long
  | 415 ->
    `Unsupported_media_type
  | 416 ->
    `Range_not_satisfiable
  | 417 ->
    `Expectation_failed
  | 418 ->
    `I_m_a_teapot
  | 420 ->
    `Enhance_your_calm
  | 421 ->
    `Misdirected_request
  | 426 ->
    `Upgrade_required
  (* Server error *)
  | 500 ->
    `Internal_server_error
  | 501 ->
    `Not_implemented
  | 502 ->
    `Bad_gateway
  | 503 ->
    `Service_unavailable
  | 504 ->
    `Gateway_timeout
  | 505 ->
    `Http_version_not_supported
  | c ->
    `Code c

let unsafe_of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
    if c < 0 then
      failwith (Printf.sprintf "Status.unsafe_of_code: %d is negative" c)
    else
      `Code c
  | s ->
    s

let of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
    if c < 100 || c > 999 then
      failwith
        (Printf.sprintf "Status.of_code: %d is not a three-digit number" c)
    else
      `Code c
  | s ->
    s

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
