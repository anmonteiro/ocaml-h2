open Http2af

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
     : socket
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Lwt.t

  val shutdown_send : socket -> unit

  val shutdown_receive : socket -> unit

  val close : socket -> unit Lwt.t

  val report_exn : Http2af.Server_connection.t -> socket -> exn -> unit Lwt.t
end


(* The function that results from [create_connection_handler] should be passed
   to [Lwt_io.establish_server_with_client_socket]. For an example, see
   [examples/lwt_echo_server.ml]. *)
module Server (Io: IO) : sig
  val create_connection_handler
    :  ?config         : Config.t
    -> request_handler : (Io.addr -> Server_connection.request_handler)
    -> error_handler   : (Io.addr -> Server_connection.error_handler)
    -> Io.addr
    -> Io.socket
    -> unit Lwt.t
end

(* For an example, see [examples/lwt_get.ml]. *)
(* module Client : sig
  val request
    :  ?config          : Config.t
    -> Lwt_unix.file_descr
    -> Request.t
    -> error_handler    : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
    -> [`write] Body.t

  module TLS : sig
    val request
      :  ?client          : Tls_io.client
      -> ?config          : Config.t
      -> Lwt_unix.file_descr
      -> Request.t
      -> error_handler    : Client_connection.error_handler
      -> response_handler : Client_connection.response_handler
      -> [`write] Body.t
  end

  module SSL : sig
    val request
      :  ?client          : Ssl_io.client
      -> ?config          : Config.t
      -> Lwt_unix.file_descr
      -> Request.t
      -> error_handler    : Client_connection.error_handler
      -> response_handler : Client_connection.response_handler
      -> [`write] Body.t
  end
end *)
