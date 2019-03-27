type t =
  { status  : Status.t
  ; headers : Headers.t }

(* From RFC7540§8.1.2.4:
     HTTP/2 does not define a way to carry the version or reason phrase that
     is included in an HTTP/1.1 status line. *)
let create ?(headers=Headers.empty) status =
  { status; headers }

let pp_hum fmt { status; headers } =
  let reason =
    match status with
    | #Status.standard as status -> Status.default_reason_phrase status
    | `Code _                    -> "Non-standard status code"
  in
  Format.fprintf fmt "((status %a) (reason %S) (headers %a))"
    Status.pp_hum (status :> Httpaf.Status.t) reason Headers.pp_hum headers
