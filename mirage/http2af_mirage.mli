open Http2af

module type Server_intf = sig
  val create_connection_handler
    :  ?config : Config.t
    -> request_handler : Server_connection.request_handler
    -> error_handler : Server_connection.error_handler
    -> (Conduit_mirage.Flow.flow -> unit Lwt.t)
end

module Server : Server_intf

module Server_with_conduit : sig
  include Server_intf

  type t = Conduit_mirage.Flow.flow -> unit Lwt.t

  val connect:
    Conduit_mirage.t ->
    (Conduit_mirage.server -> t -> unit Lwt.t) Lwt.t
end

(* For an example, see [examples/lwt_get.ml]. *)
(* module Client : sig
  val request
    :  ?config : Config.t
    -> Conduit_mirage.Flow.flow
    -> Request.t
    -> error_handler : Client_connection.error_handler
    -> response_handler : Client_connection.response_handler
      -> [`write] Httpaf.Body.t
end *)
