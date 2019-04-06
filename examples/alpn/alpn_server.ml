let start_http_server () =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 80)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        Http1_handler.redirect_handler
      >>= fun _server -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  forever

let start_https_server () =
  let open Lwt.Infix in
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 443)) in
  let cert = "./certificates/server.pem" in
  let priv_key = "./certificates/server.key" in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        (fun client_addr fd ->
          X509_lwt.private_of_pems ~cert ~priv_key >>= fun certificate ->
          let config =
            Tls.Config.server
              ~alpn_protocols:
                [ "h2"; "http/1.1" ] (* accept h2 before http/1.1 *)
              ~certificates:
                (`Single certificate)
                (* ~version:Tls.Core.(TLS_1_2, TLS_1_2) *)
              ~ciphers:
                (List.filter
                   Tls.Ciphersuite.ciphersuite_tls12_only
                   Tls.Config.Ciphers.supported)
              ()
          in
          Tls_lwt.Unix.server_of_fd config fd >>= fun tls_server ->
          match Tls_lwt.Unix.epoch tls_server with
          | `Error ->
            assert false
          | `Ok { alpn_protocol; _ } ->
            (match alpn_protocol with
            | None ->
              assert false
            | Some "http/1.1" ->
              Http1_handler.connection_handler tls_server client_addr fd
            | Some "h2" ->
              H2_handler.connection_handler tls_server client_addr fd
            | _ ->
              assert false))
      >>= fun _server -> Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  forever

let () =
  Sys.(set_signal sigpipe Signal_ignore);
  Lwt_main.run (Lwt.join [ start_http_server (); start_https_server () ])
