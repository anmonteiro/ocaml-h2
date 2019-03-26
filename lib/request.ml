type t =
  { meth    : Httpaf.Method.t
  ; target  : string
  ; headers : Headers.t }

let create ?(headers=Headers.empty) meth target =
  { meth; target; headers }

let pp_hum fmt { meth; target; headers } =
  Format.fprintf fmt "((method \"%a\") (target %S) (headers %a))"
    Httpaf.Method.pp_hum meth target Headers.pp_hum headers
