open Mirage

(* Network configuration *)

let stack = generic_stackv4 default_network

(* Dependencies *)

let server =
  foreign "Unikernel.Make"
    (console @-> pclock @-> http2 @-> job)

let app =
  http2_server @@ conduit_direct stack

let () =
  register "h2_unikernel"
    [ server $ default_console $ default_posix_clock $ app ]
