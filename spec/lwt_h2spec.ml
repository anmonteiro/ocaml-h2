let set_interval s f =
  let timeout = Lwt_timeout.create s f in
  Lwt_timeout.start timeout

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let open H2 in
  let request_handler : Unix.sockaddr -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let request = Reqd.request request_descriptor in
    match request.target with
    (* This set of routes responds immediately without reading the request
       body *)
    | "/immediately" ->
      let response_content_type =
        match Headers.get request.headers "content-type" with
        | Some request_content_type ->
          request_content_type
        | None ->
          "application/octet-stream"
      in
      let request_body = Reqd.request_body request_descriptor in
      Body.close_reader request_body;
      let response =
        Response.create
          ~headers:(Headers.of_list [ "content-type", response_content_type ])
          `OK
      in
      Reqd.respond_with_string request_descriptor response "non-empty data."
    | _ ->
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
            match request.target with
            | "/streaming" ->
              let response_body =
                Reqd.respond_with_streaming request_descriptor response
              in
              Body.write_string response_body (String.make 100 'a');
              set_interval 1 (fun () ->
                  ignore
                  @@ Reqd.try_with request_descriptor (fun () ->
                         Body.write_string response_body " data");
                  Body.flush response_body (fun () ->
                      Body.close_writer response_body))
            | "/bigstring" ->
              let res_body = "non-empty data." in
              let bs =
                Bigstringaf.of_string
                  ~off:0
                  ~len:(String.length res_body)
                  res_body
              in
              Reqd.respond_with_bigstring request_descriptor response bs
            | "/string" | _ ->
              Reqd.respond_with_string
                request_descriptor
                response
                "non-empty data.")
          ~on_read:(fun _request_data ~off:_ ~len:_ -> respond ())
      in
      respond ()
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
  H2_lwt_unix.Server.create_connection_handler
    ~config:{ H2.Config.default with max_concurrent_streams = 2l }
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
      Printf.printf "Server listening on port %i\n%!" !port;
      Lwt.return_unit);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
