open Httpaf

let redirect_handler : Unix.sockaddr -> Reqd.t Gluten.reqd -> unit =
 fun _client_address { Gluten.reqd; _ } ->
  let response =
    Response.create
      ~headers:
        (Headers.of_list
           [ "Location", "https://localhost:9443"; "Connection", "close" ])
      `Moved_permanently
  in
  Reqd.respond_with_string reqd response ""

let redirect_error_handler
    :  Unix.sockaddr -> ?request:Request.t -> _
    -> (Headers.t -> [ `write ] Body.t) -> unit
  =
 fun _client_address ?request:_ _error start_response ->
  let response_body = start_response Headers.empty in
  Body.close_writer response_body

let request_handler : Unix.sockaddr -> Reqd.t Gluten.reqd -> unit =
 fun _client_address { Gluten.reqd; _ } ->
  let request = Reqd.request reqd in
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
  Reqd.respond_with_string reqd response response_body

let error_handler
    :  Unix.sockaddr -> ?request:Request.t -> _
    -> (Headers.t -> [ `write ] Body.t) -> unit
  =
 fun _client_address ?request:_ _error start_response ->
  let response_body = start_response Headers.empty in
  Body.close_writer response_body
