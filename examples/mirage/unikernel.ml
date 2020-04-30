open Lwt.Infix
open H2

module type HTTP2 = H2_mirage.Server

module Dispatch (C : Mirage_console.S) (Http2 : HTTP2) = struct
  let log c fmt = Printf.ksprintf (C.log c) fmt

  let get_content c path =
    log c "Replying: %s" path >|= fun () -> "Hello from the h2 unikernel"

  let dispatcher c reqd =
    let { Request.target; _ } = Reqd.request reqd in
    Lwt.catch
      (fun () ->
        get_content c target >|= fun body ->
        let response =
          Response.create
            ~headers:
              (Headers.of_list
                 [ "content-length", body |> String.length |> string_of_int ])
            `OK
        in
        Reqd.respond_with_string reqd response body)
      (fun exn ->
        let response = Response.create `Internal_server_error in
        Lwt.return
          (Reqd.respond_with_string reqd response (Printexc.to_string exn)))
    |> ignore

  let serve c dispatch =
    let error_handler ?request:_ _error mk_response =
      let response_body = mk_response Headers.empty in
      Body.write_string response_body "Error handled";
      Body.flush response_body (fun () -> Body.close_writer response_body)
    in
    Http2.create_connection_handler
      ?config:None
      ~request_handler:(dispatch c)
      ~error_handler
end

(** Server boilerplate *)
module Make (C : Mirage_console.S) (Clock : Mirage_clock.PCLOCK) (Http2 : HTTP2) =
struct
  module D = Dispatch (C) (Http2)

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let start c _clock http2 =
    log c "started unikernel listen on port 8001" >>= fun () ->
    http2 (`TCP 8001) @@ D.serve c D.dispatcher
end
