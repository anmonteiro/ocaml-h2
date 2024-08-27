let set_interval ~clock s f =
  Eio.Time.sleep clock s;
  f ()

let connection_handler ~sw ~clock =
  let open H2 in
  let request_handler : Eio.Net.Sockaddr.stream -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let request = Reqd.request request_descriptor in
    match request.target with
    (* This set of routes responds immediately without reading the request
       body *)
    | "/immediately" ->
      let response_content_type =
        match Headers.get request.headers "content-type" with
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in
      let request_body = Reqd.request_body request_descriptor in
      Body.Reader.close request_body;
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
        | Some request_content_type -> request_content_type
        | None -> "application/octet-stream"
      in
      let rec respond () =
        Body.Reader.schedule_read
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
              Body.Writer.write_string response_body (String.make 100 'a');
              Eio.Fiber.fork ~sw (fun () ->
                set_interval ~clock 1. (fun () ->
                  ignore
                  @@ Reqd.try_with request_descriptor (fun () ->
                    Body.Writer.write_string response_body " data");
                  Body.Writer.flush response_body (fun _reason ->
                    Body.Writer.close response_body)))
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
  let error_handler :
       Eio.Net.Sockaddr.stream
      -> ?request:H2.Request.t
      -> _
      -> (Headers.t -> Body.Writer.t)
      -> unit
    =
   fun _client_address ?request:_ error start_response ->
    let response_body = start_response Headers.empty in
    (match error with
    | `Exn exn ->
      Body.Writer.write_string response_body (Printexc.to_string exn);
      Body.Writer.write_string response_body "\n"
    | #Status.standard as error ->
      Body.Writer.write_string
        response_body
        (Status.default_reason_phrase error));
    Body.Writer.close response_body
  in
  H2_eio.Server.create_connection_handler
    ~sw
    ~config:{ H2.Config.default with max_concurrent_streams = 2l }
    ~request_handler
    ~error_handler

let () =
  Sys.(set_signal sigpipe Signal_ignore);
  let port = ref 8080 in
  Arg.parse
    [ "-p", Arg.Set_int port, " Listening port number (8080 by default)" ]
    ignore
    "Echoes POST requests. Runs forever.";

  let listen_address = `Tcp (Eio.Net.Ipaddr.V4.loopback, !port) in
  Eio_main.run (fun env ->
    let network = Eio.Stdenv.net env in
    let clock = Eio.Stdenv.clock env in
    Eio.Switch.run (fun sw ->
      let socket =
        Eio.Net.listen
          ~reuse_addr:true
          ~reuse_port:true
          ~backlog:5
          ~sw
          network
          listen_address
      in
      let domain_mgr = Eio.Stdenv.domain_mgr env in
      let p, _ = Eio.Promise.create () in
      for _i = 1 to Domain.recommended_domain_count () do
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Eio.Domain_manager.run domain_mgr (fun () ->
            Eio.Switch.run (fun sw ->
              while true do
                Eio.Net.accept_fork
                  socket
                  ~sw
                  ~on_error:raise
                  (fun client_sock client_addr ->
                     (* let p, u = Eio.Promise.create () in *)
                     connection_handler ~sw ~clock client_addr client_sock)
              done;
              `Stop_daemon)))
      done;
      Eio.Promise.await p))
