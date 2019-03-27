let set_interval s f =
  let timeout =
    Lwt_timeout.create s f in
  Lwt_timeout.start timeout

let connection_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let open H2 in

  let request_handler : Unix.sockaddr -> Reqd.t -> unit =
      fun _client_address request_descriptor ->

    let request = Reqd.request request_descriptor in
    match request.meth, request.target with
    (* This set of routes waits until the entire request body has been read
     * to produce a response. *)
    | `GET, "/"
    | `POST, "/" ->
      let request_body = Reqd.request_body request_descriptor in
      let response_content_type =
        match Headers.get request.headers "content-type" with
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in

      let rec respond () =
        Body.schedule_read
          request_body
          ~on_eof:(fun () ->
            let response =
              Response.create
                ~headers:(Headers.of_list [
                  "content-type", response_content_type;
                  "content-length", "100"
                ])
                `OK
            in

            let response_body =
              Reqd.respond_with_streaming request_descriptor response
            in
            Body.write_string response_body (String.make 100 'a');
            (* Body.close_writer response_body *)
              (* Body.flush response_body (fun () ->
            Body.close_writer response_body
                 ); *)
              set_interval 1 (fun () ->
                ignore @@ Reqd.try_with request_descriptor (fun () ->
                  Body.write_string response_body " data");
                Body.flush response_body (fun () ->
                  Body.close_writer response_body)))
          ~on_read:(fun _request_data ~off:_ ~len:_ ->
            respond ())
      in
      respond ()

    (* This set of routes responds immediately without reading the request body *)
    | `GET, "/immediately"
    | `POST, "/immediately" ->
      let response_content_type =
        match Headers.get request.headers "content-type" with
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in
      let response =
        Response.create
          ~headers:(Headers.of_list [
            "content-type", response_content_type;
          ])
          `OK
      in

      let response_body =
        Reqd.respond_with_streaming request_descriptor response
      in
      Body.write_string response_body "non-empty data.";
      Body.flush response_body (fun () ->
        Body.close_writer response_body)

    | _ ->
      Reqd.respond_with_string
        request_descriptor (Response.create `Method_not_allowed) ""
  in

  let error_handler :
      Unix.sockaddr ->
      ?request:H2.Request.t ->
      _ ->
      (Headers.t -> [`write] Body.t) ->
        unit =
      fun _client_address ?request:_ error start_response ->

    let response_body = start_response Headers.empty in

    begin match error with
    | `Exn exn ->
      Body.write_string response_body (Printexc.to_string exn);
      Body.write_string response_body "\n";

    | #Status.standard as error ->
      Body.write_string response_body (Status.default_reason_phrase error)
    end;

    Body.close_writer response_body
  in

  H2_lwt_unix.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler


let () =
  let open Lwt.Infix in
  Sys.(set_signal sigpipe Signal_ignore);
  let port = ref 8080 in
  Arg.parse
    ["-p", Arg.Set_int port, " Listening port number (8080 by default)"]
    ignore
    "Echoes POST requests. Runs forever.";

  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, !port)) in

  Lwt.async begin fun () ->
    Lwt_io.establish_server_with_client_socket
      listen_address connection_handler
    >>= fun _server ->
      Printf.printf "Listening on port %i and echoing POST requests.\n" !port;
      print_string "To send a POST request, try\n\n";
      print_string "  echo foo | dune exec examples/lwt/lwt_post.exe\n\n";
      flush stdout;
      Lwt.return_unit
  end;

  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
