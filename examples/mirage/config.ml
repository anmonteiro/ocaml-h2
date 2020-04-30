open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Dependencies *)

let server =
  let packages =
    [ package ~pin:"file://../../" "h2-lwt"
    ; package ~pin:"file://../../" "h2-mirage"
    ]
  in
  foreign "Unikernel.Make" ~packages (console @-> pclock @-> http2 @-> job)

let app = http2_server @@ conduit_direct stack

let () =
  register
    "h2_unikernel"
    [ server $ default_console $ default_posix_clock $ app ]
