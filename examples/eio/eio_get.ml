open H2
module Client = H2_eio.Client

let response_handler ~on_eof response response_body =
  Format.eprintf "Response: %a@." Response.pp_hum response;

  let rec read_response () =
    Body.Reader.schedule_read
      response_body
      ~on_eof
      ~on_read:(fun bigstring ~off ~len ->
        Format.eprintf "heh nice %d@." len;
        let response_fragment = Bytes.create len in
        Bigstringaf.blit_to_bytes
          bigstring
          ~src_off:off
          response_fragment
          ~dst_off:0
          ~len;
        print_string (Bytes.to_string response_fragment);
        read_response ())
  in
  read_response ()

let error_handler err =
  match err with
  | `Exn exn -> Format.eprintf "wut %S@." (Printexc.to_string exn)
  | `Invalid_response_body_length res ->
    Format.eprintf "invalid res: %a@." Response.pp_hum res
  | `Malformed_response str -> Format.eprintf "malformed %S@." str
  | `Protocol_error (err, s) ->
    Format.eprintf "wut %a %s@." H2.Error_code.pp_hum err s

let () =
  Ssl_threads.init ();
  Ssl.init ~thread_safe:true ();
  let host = ref None in
  let port = ref 443 in
  Arg.parse
    [ "-p", Set_int port, " Port number (443 by default)" ]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eio_main.run (fun env ->
      let network = Eio.Stdenv.net env in
      Eio.Switch.run (fun sw ->
          let addrs =
            let addrs =
              Eio_unix.run_in_systhread (fun () ->
                  Unix.getaddrinfo
                    host
                    (string_of_int !port)
                    [ Unix.(AI_FAMILY PF_INET) ])
            in
            List.filter_map
              (fun (addr : Unix.addr_info) ->
                match addr.ai_addr with
                | Unix.ADDR_UNIX _ -> None
                | ADDR_INET (addr, port) -> Some (addr, port))
              addrs
          in
          let addr =
            let inet, port = List.hd addrs in
            `Tcp (Eio_unix.Ipaddr.of_unix inet, port)
          in
          let socket = Eio.Net.connect ~sw network addr in

          let request =
            Request.create
              `GET
              "/"
              ~scheme:"https"
              ~headers:Headers.(add_list empty [ ":authority", host ])
          in

          let ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
          Ssl.disable_protocols ctx [ Ssl.SSLv23 ];
          Ssl.honor_cipher_order ctx;
          Ssl.set_context_alpn_protos ctx [ "h2" ];
          let ssl_ctx = Eio_ssl.Context.create ~ctx socket in
          let ssl_sock = Eio_ssl.Context.ssl_socket ssl_ctx in
          Ssl.set_client_SNI_hostname ssl_sock host;
          Ssl.set_hostflags ssl_sock [ No_partial_wildcards ];
          Ssl.set_host ssl_sock host;
          let ssl_sock = Eio_ssl.connect ssl_ctx in

          let connection =
            Client.create_connection
              ~sw
              ~error_handler
              (ssl_sock :> Eio.Flow.two_way)
          in
          let response_handler =
            response_handler ~on_eof:(fun () ->
                Format.eprintf "eof@.";
                Client.shutdown connection)
          in
          let request_body =
            Client.request
              connection
              request
              ~error_handler
              ~response_handler
              ~flush_headers_immediately:true
          in
          Body.Writer.close request_body))
