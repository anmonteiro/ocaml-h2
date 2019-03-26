module Io : Http2af_lwt.IO with
    type socket = Lwt_unix.file_descr * [ `Tls_not_available ]
    and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr * [ `Tls_not_available ]
  type addr = Unix.sockaddr

  let read _ _bigstring ~off:_ ~len:_ =
    Lwt.fail_with "Tls not available"

  let writev _ = fun _iovecs ->
    Lwt.fail_with "Tls not available"

  let shutdown_send _ =
    failwith "Tls not available"

  let shutdown_receive _ =
    failwith "Tls not available"

  let close _ =
    Lwt.fail_with "Tls not available"

  let report_exn _connection _ = fun _exn ->
    Lwt.fail_with "Tls not available"
end

type client = [ `Tls_not_available ]
type server = [ `Tls_not_available ]

let make_client ?client:_ _socket =
  Lwt.fail_with "Tls not available"

let make_server ?server:_ ?certfile:_ ?keyfile:_ _socket =
  Lwt.fail_with "Tls not available"
