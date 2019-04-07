open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Certificates *)

let secrets = generic_kv_ro "../../../certificates"

(* Dependencies *)

let server =
  let packages =
    [ package ~sublibs:[ "mirage" ] "tls"
    ; package ~pin:"git+https://github.com/anmonteiro/httpaf#mirage" "httpaf"
    ; package
        ~pin:"git+https://github.com/anmonteiro/httpaf#mirage"
        "httpaf-mirage"
    ; package
        ~pin:"git+https://github.com/anmonteiro/httpaf#mirage"
        "httpaf-lwt"
      (* Just because 0.2.0 is not merged in opam *)
    ; package ~pin:"git+https://github.com/anmonteiro/ocaml-h2" "h2"
    ; package ~pin:"git+https://github.com/anmonteiro/ocaml-h2" "h2-lwt"
    ; package ~pin:"git+https://github.com/anmonteiro/ocaml-h2" "h2-mirage"
    ]
  in
  foreign
    ~packages
    ~deps:[ abstract nocrypto ]
    "Unikernel.Make"
    (stackv4 @-> kv_ro @-> console @-> pclock @-> job)

let () =
  register
    "alpn_unikernel"
    [ server $ stack $ secrets $ default_console $ default_posix_clock ]
