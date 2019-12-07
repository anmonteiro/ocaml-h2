let set_interval ?(times = 5) s f destroy =
  let rec set_interval_loop s f n =
    let timeout =
      Lwt_timeout.create s (fun () ->
          if n > 0 then (
            if f () then
              set_interval_loop s f (n - 1))
          else
            destroy ())
    in
    Lwt_timeout.start timeout
  in
  set_interval_loop s f times

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let open H2 in
  let request_handler : Unix.sockaddr -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let request = Reqd.request request_descriptor in
    match request.meth, request.target with
    | `POST, "/" ->
      (* let request_body = Reqd.request_body request_descriptor in *)
      let response_content_type =
        match Headers.get request.headers "Content-Type" with
        | Some request_content_type ->
          request_content_type
        | None ->
          "application/octet-stream"
      in
      let response =
        Response.create
          ~headers:
            (Headers.of_list
               [ "content-type", response_content_type
                 (* "Connection", "close"; *)
               ])
          `OK
      in
      (* let response_body = Reqd.respond_with_streaming request_descriptor
         response in *)

      (* let rec respond () = Body.schedule_read request_body ~on_eof:(fun () ->
         Body.close_writer response_body) ~on_read:(fun request_data ~off ~len
         -> Body.write_bigstring response_body request_data ~off ~len; respond
         ()) in respond () *)
      Reqd.respond_with_string request_descriptor response "ANTOINO"
    | `POST, "/foo" ->
      set_interval
        ~times:1
        2
        (fun () ->
          let response_content_type =
            match Headers.get request.headers "Content-Type" with
            | Some request_content_type ->
              request_content_type
            | None ->
              "application/octet-stream"
          in
          let response =
            Response.create
              ~headers:
                (Headers.of_list
                   [ "content-type", response_content_type
                     (* "Connection", "close"; *)
                   ])
              `OK
          in
          Reqd.respond_with_string request_descriptor response "ANTOINO";
          true)
        ignore
    | `GET, _ ->
      Reqd.respond_with_string
        request_descriptor
        (Response.create `OK)
        "Welcome to ocaml-h2"
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
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    (* begin match error with | `Exn exn -> Body.write_string response_body
       (Printexc.to_string exn); Body.write_string response_body "\n";

       | #Status.standard as error -> Body.write_string response_body
       (Status.default_reason_phrase error) end; *)
    Body.close_writer response_body
  in
  let certfile = "./certificates/server.pem" in
  let keyfile = "./certificates/server.key" in
  H2_lwt_unix.Server.SSL.create_connection_handler_with_default
    ~certfile
    ~keyfile
    ?config:None
    ~request_handler
    ~error_handler

let () =
  let open Lwt.Infix in
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
      print_string "  curl https://localhost:8080 -k -X POST -d foo\n\n";
      flush stdout;
      Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
