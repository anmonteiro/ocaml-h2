open Alpn_lib

let http1_handler =
  Httpaf_lwt_unix.Server.SSL.create_connection_handler
    ?config:None
    ~request_handler:Http1_handler.request_handler
    ~error_handler:Http1_handler.error_handler

let h2_handler =
  H2_lwt_unix.Server.SSL.create_connection_handler
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

let rec first_match l1 = function
  | [] ->
    None
  | x :: _ when List.mem x l1 ->
    Some x
  | _ :: xs ->
    first_match l1 xs

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
              let server_ctx =
                Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context
              in
              Ssl.disable_protocols server_ctx [ Ssl.SSLv23; Ssl.TLSv1_1 ];
              Ssl.use_certificate server_ctx cert priv_key;
              let protos = [ "h2"; "http/1.1" ] in
              Ssl.set_context_alpn_protos server_ctx protos;
              Ssl.set_context_alpn_select_callback
                server_ctx
                (fun client_protos -> first_match client_protos protos);
              Lwt_ssl.ssl_accept fd server_ctx >>= fun ssl_server ->
              match Lwt_ssl.ssl_socket ssl_server with
              | None ->
                assert false
              | Some ssl_socket ->
                (match Ssl.get_negotiated_alpn_protocol ssl_socket with
                | None ->
                  (* Unable to negotiate a protocol *)
                  Lwt.return_unit
                | Some "http/1.1" ->
                  http1_handler client_addr ssl_server
                | Some "h2" ->
                  h2_handler client_addr ssl_server
                | Some _ ->
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
