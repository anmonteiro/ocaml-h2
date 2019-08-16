open H2

module type IO = sig
  type socket

  type addr

  val read
    :  socket
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> [ `Eof | `Ok of int ] Lwt.t

  val writev
    :  socket
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Lwt.t

  val shutdown_send : socket -> unit

  val shutdown_receive : socket -> unit

  val close : socket -> unit Lwt.t

  val report_exn : Server_connection.t -> socket -> exn -> unit Lwt.t
end

module type Server = sig
  type socket

  type addr

  val create_connection_handler
    :  ?config:Config.t
    -> request_handler:(addr -> Server_connection.request_handler)
    -> error_handler:(addr -> Server_connection.error_handler)
    -> addr
    -> socket
    -> unit Lwt.t
end

module type Client = sig
  type t

  type socket

  val create_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> error_handler:Client_connection.error_handler
    -> socket
    -> t Lwt.t

  val request
    :  t
    -> Request.t
    -> error_handler:Client_connection.error_handler
    -> response_handler:Client_connection.response_handler
    -> [ `write ] Body.t

  val ping : t -> ?payload:Bigstringaf.t -> ?off:int -> (unit -> unit) -> unit

  val shutdown : t -> unit

  val is_closed : t -> bool
end
