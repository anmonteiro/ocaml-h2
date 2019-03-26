(* if we ever drop support for OCaml < 4.06 we can use a variant of the
   following, and remove all the function forwarding.

   From https://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec249:

     Prior to OCaml 4.06, there were a number of restrictions: one could only
     remove types and modules at the outermost level (not inside submodules),
     and in the case of with type the definition had to be another type
     constructor with the same type parameters. *)
(* include (Httpaf.Status : Status with type informational := [ | `Continue ]) *)

(* From RFC7540§8.1.1:
     HTTP/2 removes support for the 101 (Switching Protocols) informational
     status code ([RFC7231], Section 6.2.2). *)
type informational = [ | `Continue ]

type successful = Httpaf.Status.successful
type redirection = Httpaf.Status.redirection
type client_error = Httpaf.Status.client_error
type server_error = Httpaf.Status.server_error

type standard = [
  | informational
  | Httpaf.Status.successful
  | Httpaf.Status.redirection
  | Httpaf.Status.client_error
  | Httpaf.Status.server_error
  ]

type t = [
  | standard
  | `Code of int ]

let default_reason_phrase t =
  Httpaf.Status.default_reason_phrase (t :> Httpaf.Status.standard)

let really_unsafe_of_code = function
 (* Informational *)
  | 100 -> `Continue
 (* Successful *)
  | 200 -> `OK
  | 201 -> `Created
  | 202 -> `Accepted
  | 203 -> `Non_authoritative_information
  | 204 -> `No_content
  | 205 -> `Reset_content
  | 206 -> `Partial_content
 (* Redirection *)
  | 300 -> `Multiple_choices
  | 301 -> `Moved_permanently
  | 302 -> `Found
  | 303 -> `See_other
  | 304 -> `Not_modified
  | 305 -> `Use_proxy
  | 307 -> `Temporary_redirect
 (* Client error *)
  | 400 -> `Bad_request
  | 401 -> `Unauthorized
  | 402 -> `Payment_required
  | 403 -> `Forbidden
  | 404 -> `Not_found
  | 405 -> `Method_not_allowed
  | 406 -> `Not_acceptable
  | 407 -> `Proxy_authentication_required
  | 408 -> `Request_timeout
  | 409 -> `Conflict
  | 410 -> `Gone
  | 411 -> `Length_required
  | 412 -> `Precondition_failed
  | 413 -> `Payload_too_large
  | 414 -> `Uri_too_long
  | 415 -> `Unsupported_media_type
  | 416 -> `Range_not_satisfiable
  | 417 -> `Expectation_failed
  | 418 -> `I_m_a_teapot
  | 420 -> `Enhance_your_calm
  | 426 -> `Upgrade_required
 (* Server error *)
  | 500 -> `Internal_server_error
  | 501 -> `Not_implemented
  | 502 -> `Bad_gateway
  | 503 -> `Service_unavailable
  | 504 -> `Gateway_timeout
  | 505 -> `Http_version_not_supported
  | c   -> `Code c
let to_code t =
  Httpaf.Status.to_code (t :> Httpaf.Status.t)

let unsafe_of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
    if c < 0
    then failwith (Printf.sprintf "Status.unsafe_of_code: %d is negative" c)
    else `Code c
  | s -> s

let of_code c =
  match really_unsafe_of_code c with
  | `Code c ->
    if c < 100 || c > 999
    then failwith (Printf.sprintf "Status.of_code: %d is not a three-digit number" c)
    else `Code c
  | s -> s

let is_informational t =
  match t with
  | #informational -> true
  | `Code n        -> n >= 100 && n <= 199
  | _              -> false

let is_successful t =
  Httpaf.Status.is_successful (t :> Httpaf.Status.t)

let is_redirection t =
  Httpaf.Status.is_redirection (t :> Httpaf.Status.t)

let is_client_error t =
  Httpaf.Status.is_client_error (t :> Httpaf.Status.t)

let is_server_error t =
  Httpaf.Status.is_server_error (t :> Httpaf.Status.t)

let is_error t =
  Httpaf.Status.is_error (t :> Httpaf.Status.t)


let to_string t =
  Httpaf.Status.to_string (t :> Httpaf.Status.t)

let of_string x =
  of_code (int_of_string x)

let pp_hum fmt t =
  Httpaf.Status.pp_hum fmt (t :> Httpaf.Status.t)
