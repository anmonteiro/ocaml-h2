open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Certificates *)

let secrets = generic_kv_ro "../../../certificates"

(* Dependencies *)

let server =
  let packages =
    [ package ~sublibs:[ "mirage" ] "tls"
    ; package ~pin:"git+https://github.com/anmonteiro/httpaf#fork" "httpaf"
    ; package ~pin:"git+https://github.com/anmonteiro/httpaf#fork" "httpaf-lwt"
    ; package
        ~pin:"git+https://github.com/anmonteiro/httpaf#fork"
        "httpaf-mirage"
    ; package
        ~pin:"git+https://github.com/anmonteiro/httpaf#fork"
        "httpaf-lwt-unix"
    ; package "h2-mirage"
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
