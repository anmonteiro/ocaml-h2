open Mirage

(* Network configuration *)

let secrets_key = Key.(value @@ kv_ro ~group:"secrets" ())
let secrets = generic_kv_ro ~key:secrets_key "./certificates"
let stack = generic_stackv4 default_network

(* TLS *)
let tls_key =
  let doc = Key.Arg.info
      ~doc:"Enable serving the website over https. Do not forget to put certificates in tls/"
      ~docv:"BOOL" ~env:"TLS" ["tls"]
  in
  Key.(create "tls" Arg.(opt ~stage:`Configure bool false doc))

(* Dependencies *)
let packages =
  [ package ~sublibs:["mirage"] "tls" ]

let server =
  foreign ~packages "Unikernel.Make"
    (console @-> kv_ro @-> pclock @-> http2 @-> job)

let app =
  http2_server @@ conduit_direct ~tls:true stack

let () =
  register "http2af_unikernel"
    [ server $ default_console $ secrets $ default_posix_clock $ app ]
