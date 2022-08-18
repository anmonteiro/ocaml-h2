module type Server = sig
  type socket

  val create_connection_handler
    :  ?config:H2.Config.t
    -> domain_mgr:Eio.Domain_manager.t
    -> request_handler:(Eio.Net.Sockaddr.stream -> H2.Reqd.t -> unit)
    -> error_handler:
         (Eio.Net.Sockaddr.stream -> H2.Server_connection.error_handler)
    -> Eio.Net.Sockaddr.stream
    -> socket
    -> unit
end

module type Client = sig
  type socket
  type runtime
  type t

  val create_connection
    :  ?config:H2.Config.t
    -> ?push_handler:
         (H2.Request.t -> (H2.Client_connection.response_handler, unit) result)
    -> domain_mgr:Eio.Domain_manager.t
    -> error_handler:H2.Client_connection.error_handler
    -> socket
    -> t

  val request
    :  t
    -> ?flush_headers_immediately:bool
    -> ?trailers_handler:H2.Client_connection.trailers_handler
    -> H2.Request.t
    -> error_handler:H2.Client_connection.error_handler
    -> response_handler:H2.Client_connection.response_handler
    -> H2.Body.Writer.t

  val ping : t -> ?payload:Bigstringaf.t -> ?off:int -> (unit -> unit) -> unit
  val shutdown : t -> unit
  val is_closed : t -> bool
end
