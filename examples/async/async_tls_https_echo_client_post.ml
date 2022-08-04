open Core
open Async
open H2
open H2_async

let error_handler = function
  | `Invalid_response_body_length _resp ->
    printf "invalid response body length\n%!"
  | `Exn _exn -> printf "exception!\n%!"
  | `Malformed_response s -> printf "malformed response: %s\n%!" s
  | `Protocol_error (code, s) ->
    printf "protocol error: %s, %s\n%!" (H2.Error_code.to_string code) s

let response_handler response_received_ivar response response_body =
  match Response.(response.status) with
  | `OK ->
    let rec read_response () =
      Body.Reader.schedule_read
        response_body
        ~on_eof:(fun () -> Ivar.fill response_received_ivar ())
        ~on_read:(fun response_fragment ~off ~len ->
          printf
            "Server response: %s\n%!"
            (Bigstringaf.substring ~off ~len response_fragment);
          read_response ())
    in
    read_response ()
  | _ ->
    Format.fprintf Format.err_formatter "%a\n%!" Response.pp_hum response;
    Shutdown.shutdown 1

let main port host () =
  printf "Type in your text to send, then hit EOF:\n%!";
  Reader.contents (Lazy.force Reader.stdin) >>= fun text_to_send ->
  let socket = Unix.Socket.create Unix.Socket.Type.tcp in
  let where_to_connect =
    let hnp = Host_and_port.create ~host ~port in
    Tcp.Where_to_connect.of_host_and_port hnp
  in
  Client.TLS.create_connection_with_default
    ~error_handler
    socket
    where_to_connect
  >>= fun tls_conn ->
  let request_headers =
    Request.create
      `POST
      "/"
      ~scheme:"http"
      ~headers:
        Headers.(
          of_list
            [ ":authority", host
            ; "content-length", string_of_int (String.length text_to_send)
            ])
  in
  let response_received_ivar = Ivar.create () in
  let response_handler = response_handler response_received_ivar in
  let request_body =
    H2_async.Client.TLS.request
      tls_conn
      request_headers
      ~error_handler
      ~response_handler
  in
  Body.Writer.write_string request_body text_to_send;
  Body.Writer.close request_body;
  Ivar.read response_received_ivar

let () =
  Command.async_spec
    ~summary:"Start a hello world tls-async client"
    Command.Spec.(
      empty
      +> flag
           "-port"
           (optional_with_default 8080 int)
           ~doc:"int Source port to listen on"
      +> flag
           "-host"
           (optional_with_default "localhost" string)
           ~doc:"HOST to connect to")
    main
  |> Command_unix.run
