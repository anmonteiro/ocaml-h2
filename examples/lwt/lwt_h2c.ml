open Lwt.Infix

module Http2 = struct
  let connection_handler
      :  Httpaf.Request.t -> Unix.sockaddr -> Lwt_unix.file_descr
      -> (unit, string) result Lwt.t
    =
    let open H2 in
    let request_handler : Unix.sockaddr -> Reqd.t -> unit =
     fun _client_address request_descriptor ->
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
                "non-empty data.")
            ~on_read:(fun _request_data ~off:_ ~len:_ -> respond ())
        in
        respond ()
      | _ ->
        Reqd.respond_with_string
          request_descriptor
          (Response.create `Method_not_allowed)
          ""
    in
    let error_handler
        :  Unix.sockaddr -> ?request:H2.Request.t -> _
        -> (Headers.t -> [ `write ] Body.t) -> unit
      =
     fun _client_address ?request:_ error start_response ->
      let response_body = start_response Headers.empty in
      (match error with
      | `Exn exn ->
        Body.write_string response_body (Printexc.to_string exn);
        Body.write_string response_body "\n"
      | #Status.standard as error ->
        Body.write_string response_body (Status.default_reason_phrase error));
      Body.close_writer response_body
    in
    fun http_request ->
      H2_lwt_unix.Server.create_h2c_connection_handler
        ?config:None
        ~http_request
        ~request_handler
        ~error_handler
end

let connection_handler =
  let module Body = Httpaf.Body in
  let module Headers = Httpaf.Headers in
  let module Reqd = Httpaf.Reqd in
  let module Response = Httpaf.Response in
  let module Status = Httpaf.Status in
  let upgrade_handler request addr socket =
    Http2.connection_handler request addr socket >|= ignore
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
  let request_handler addr reqd =
    let headers =
      Headers.of_list [ "Connection", "Upgrade"; "Upgrade", "h2c" ]
    in
    let request = Reqd.request reqd in
    Reqd.respond_with_upgrade reqd headers (upgrade_handler request addr)
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
