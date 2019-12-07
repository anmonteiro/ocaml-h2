open Alpn_lib

let http1_handler =
  Httpaf_lwt_unix.Server.TLS.create_connection_handler
    ?config:None
    ~request_handler:Http1_handler.request_handler
    ~error_handler:Http1_handler.error_handler

let h2_handler =
  H2_lwt_unix.Server.TLS.create_connection_handler
    ?config:None
    ~request_handler:H2_handler.request_handler
    ~error_handler:H2_handler.error_handler

let start_http_server () =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        (Httpaf_lwt_unix.Server.create_connection_handler
           ?config:None
           ~request_handler:Http1_handler.redirect_handler
           ~error_handler:Http1_handler.redirect_error_handler)
      >>= fun _server -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  forever

let start_https_server () =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 9443)) in
  let cert = "./certificates/server.pem" in
  let priv_key = "./certificates/server.key" in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        (fun client_addr fd ->
          Lwt.catch
            (fun () ->
              X509_lwt.private_of_pems ~cert ~priv_key >>= fun certificate ->
              let config =
                Tls.Config.server
                  ~alpn_protocols:
                    [ "h2"; "http/1.1" ] (* accept h2 before http/1.1 *)
                  ~certificates:(`Single certificate)
                  ~ciphers:
                    (List.filter
                       Tls.Ciphersuite.ciphersuite_tls12_only
                       Tls.Config.Ciphers.supported)
                  ()
              in
              Tls_lwt.Unix.server_of_fd config fd >>= fun tls_server ->
              match Tls_lwt.Unix.epoch tls_server with
              | `Error ->
                Lwt_io.eprintlf
                  "Unable to fetch session data. Did the handshake fail?"
              | `Ok { alpn_protocol; _ } ->
                (match alpn_protocol with
                | None ->
                  (* Unable to negotiate a protocol *)
                  Lwt.return_unit
                | Some "http/1.1" ->
                  http1_handler client_addr tls_server
                | Some "h2" ->
                  h2_handler client_addr tls_server
                | _ ->
                  (* Can't really happen - would mean that TLS negotiated a
                   * protocol that we didn't specify. *)
                  assert false))
            (fun exn -> Lwt_io.eprintlf "EXN: %s" (Printexc.to_string exn)))
      >>= fun _server -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  forever

let () =
  Sys.(set_signal sigpipe Signal_ignore);
  Lwt_main.run (Lwt.join [ start_http_server (); start_https_server () ])
