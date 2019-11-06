open H2

let request_handler : Reqd.t -> unit =
 fun request_descriptor ->
  let { Request.headers; _ } = Reqd.request request_descriptor in
  let response_content_type =
    match Headers.get headers "Content-Type" with
    | Some request_content_type ->
      request_content_type
    | None ->
      "application/octet-stream"
  in
  let response =
    Response.create
      ~headers:(Headers.of_list [ "content-type", response_content_type ])
      `OK
  in
  Reqd.respond_with_string
    request_descriptor
    response
    "Welcome to an ALPN-negotiated HTTP/2 connection"

let error_handler
    : ?request:H2.Request.t -> _ -> (Headers.t -> [ `write ] Body.t) -> unit
  =
 fun ?request:_ _error start_response ->
  let response_body = start_response Headers.empty in
  Body.close_writer response_body
