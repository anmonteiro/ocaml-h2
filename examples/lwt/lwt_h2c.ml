open Lwt.Infix

module Http2 = struct
  open H2

  let connection_handler
      :  Httpaf.Request.t -> Bigstringaf.t H2.IOVec.t list
      -> (Server_connection.t, string) result
    =
    let request_handler : H2.Server_connection.request_handler =
     fun request_descriptor ->
      let request = Reqd.request request_descriptor in
      match request.meth, request.target with
      | `GET, "/" | `POST, "/" ->
        (* This set of routes waits until the entire request body has been read
         * to produce a response. *)
        let request_body = Reqd.request_body request_descriptor in
        let response_content_type =
          match Headers.get request.headers "content-type" with
          | Some request_content_type ->
            request_content_type
          | None ->
            "application/octet-stream"
        in
        let buf = Buffer.create 10 in
        let rec respond () =
          Body.schedule_read
            request_body
            ~on_eof:(fun () ->
              let response =
                Response.create
                  ~headers:
                    (Headers.of_list [ "content-type", response_content_type ])
                  `OK
              in
              Reqd.respond_with_string
                request_descriptor
                response
                (Buffer.contents buf))
            ~on_read:(fun request_data ~off ~len ->
              let bytes = Bytes.create len in
              Bigstringaf.blit_to_bytes
                request_data
                ~src_off:off
                ~dst_off:0
                ~len
                bytes;
              Buffer.add_bytes buf bytes;
              respond ())
        in
        respond ()
      | _ ->
        Reqd.respond_with_string
          request_descriptor
          (Response.create `Method_not_allowed)
          ""
    in
    let error_handler ?request:_ error start_response =
      let response_body = start_response Headers.empty in
      (match error with
      | `Exn exn ->
        Body.write_string response_body (Printexc.to_string exn);
        Body.write_string response_body "\n"
      | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error));
      Body.close_writer response_body
    in
    fun http_request request_body ->
      H2.Server_connection.create_h2c
        ?config:None
        ~http_request
        ~request_body
        ~error_handler
        request_handler
end

let connection_handler =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in
  let upgrade_handler request upgrade () =
    let off = 0 in
    let len = 3 in
    let body =
      [ { H2.IOVec.buffer = Bigstringaf.of_string ~off ~len "foo"; off; len }
      ; { buffer = Bigstringaf.of_string ~off ~len "bar"; off; len }
      ; { buffer = Bigstringaf.of_string ~off ~len "baz"; off; len }
      ]
    in
    let connection =
      Stdlib.Result.get_ok (Http2.connection_handler request body)
    in
    upgrade (Gluten.make (module H2.Server_connection) connection)
  in
  let http_error_handler _client_address ?request:_ error handle =
    let message =
      match error with
      | `Exn exn ->
        Printexc.to_string exn
      | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
    in
    let body = handle Headers.empty in
    Body.write_string body message;
    Body.close_writer body
  in
  let request_handler _addr (reqd : Httpaf.Reqd.t Gluten.reqd) =
    let { Gluten.reqd; upgrade } = reqd in
    let headers =
      Headers.of_list [ "Connection", "Upgrade"; "Upgrade", "h2c" ]
    in
    let request = Reqd.request reqd in
    Reqd.respond_with_upgrade reqd headers (upgrade_handler request upgrade)
  in
  Httpaf_lwt_unix.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler:http_error_handler

let () =
  Sys.(set_signal sigpipe Signal_ignore);
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        connection_handler
      >>= fun _server ->
      Printf.printf "Listening on port %i and echoing POST requests.\n" !port;
      print_string "To send a POST request, try\n\n";
      print_string "  echo foo | dune exec examples/lwt/lwt_post.exe\n\n";
      flush stdout;
      Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
