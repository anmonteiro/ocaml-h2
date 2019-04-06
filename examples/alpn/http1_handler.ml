open Httpaf

let redirect_handler : Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t =
  let request_handler : Unix.sockaddr -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let response =
      Response.create
        ~headers:
          (Headers.of_list
             [ "Location", "https://localhost"; "Connection", "close" ])
        `Moved_permanently
    in
    Reqd.respond_with_string request_descriptor response ""
  in
  let error_handler
      :  Unix.sockaddr -> ?request:Request.t -> _
      -> (Headers.t -> [ `write ] Body.t) -> unit
    =
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    Body.close_writer response_body
  in
  Httpaf_lwt.Server.create_connection_handler
    ?config:None
    ~request_handler
    ~error_handler

let connection_handler
    : Tls_lwt.Unix.t -> Unix.sockaddr -> Lwt_unix.file_descr -> unit Lwt.t
  =
 fun tls_server ->
  let request_handler : Unix.sockaddr -> Reqd.t -> unit =
   fun _client_address request_descriptor ->
    let request = Reqd.request request_descriptor in
    let response_content_type =
      match Headers.get request.headers "Content-Type" with
      | Some request_content_type ->
        request_content_type
      | None ->
        "application/octet-stream"
    in
    let response_body = "Welcome to an ALPN-negotiated HTTP/1.1 connection" in
    let response =
      Response.create
        ~headers:
          (Headers.of_list
             [ "content-type", response_content_type
             ; "Content-Length", String.length response_body |> string_of_int
             ])
        `OK
    in
    Reqd.respond_with_string request_descriptor response response_body
  in
  let error_handler
      :  Unix.sockaddr -> ?request:Request.t -> _
      -> (Headers.t -> [ `write ] Body.t) -> unit
    =
   fun _client_address ?request:_ _error start_response ->
    let response_body = start_response Headers.empty in
    Body.close_writer response_body
  in
  Httpaf_lwt.Server.TLS.create_connection_handler
    ~server:tls_server
    ?certfile:None
    ?keyfile:None
    ?config:None
    ~request_handler
    ~error_handler
