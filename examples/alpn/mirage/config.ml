open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Certificates *)

let secrets = generic_kv_ro "../../../certificates"

(* Dependencies *)

let server =
  let packages =
    [ package "tls-mirage"
    ; package ~pin:"git+https://github.com/anmonteiro/httpaf#fork" "httpaf"
    ; package ~pin:"git+https://github.com/anmonteiro/httpaf#fork" "httpaf-lwt"
    ; package
        ~pin:"git+https://github.com/anmonteiro/httpaf#fork"
        "httpaf-mirage"
    ; package ~pin:"file://../../.." "h2"
    ; package ~pin:"file://../../.." "h2-lwt"
    ; package ~pin:"file://../../.." "h2-mirage"
    ]
  in
  foreign
    ~packages
    "Unikernel.Make"
    (random @-> stackv4 @-> kv_ro @-> console @-> pclock @-> job)

let () =
  register
    "alpn_unikernel"
    [ server
      $ default_random
      $ stack
      $ secrets
      $ default_console
      $ default_posix_clock
    ]
