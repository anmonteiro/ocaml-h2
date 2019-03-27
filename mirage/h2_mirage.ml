open Lwt.Infix

module Io : H2_lwt.IO with
    type socket = Conduit_mirage.Flow.flow
    and type addr = unit = struct
  type socket = Conduit_mirage.Flow.flow
  type addr = unit

  let shutdown flow =
    Conduit_mirage.Flow.close flow

  let shutdown_receive flow =
    Lwt.async (fun () -> shutdown flow)

  let shutdown_send flow =
    Lwt.async (fun () -> shutdown flow)

  let close flow = shutdown flow

  let read flow bigstring ~off ~len:_ =
    let open Conduit_mirage in
    Lwt.catch
      (fun () ->
        Flow.read flow >|= function
        | Ok (`Data buf) ->
          Bigstringaf.blit
            buf.buffer
            ~src_off:buf.off bigstring
            ~dst_off:off
            ~len:buf.len;
          `Ok buf.len
        | Ok `Eof -> `Eof
        | Error error ->
          raise (Failure (Format.asprintf "%a" Flow.pp_error error)))
      (fun exn ->
        shutdown flow >>= fun () ->
        Lwt.fail exn)

  let writev flow = fun iovecs ->
      let open Conduit_mirage in
      let cstruct_iovecs = List.map (fun { Faraday.buffer; off; len } ->
        Cstruct.of_bigarray ~off ~len buffer)
        iovecs
      in

      Lwt.catch
        (fun () ->
          Flow.writev flow cstruct_iovecs >|= fun x ->
          match x with
          | Ok () ->
            `Ok (Cstruct.lenv cstruct_iovecs)
          | Error `Closed ->
            `Closed
          | Error other_error ->
            raise (Failure (Format.asprintf "%a" Flow.pp_write_error other_error)))
        (fun exn ->
          shutdown flow >>= fun () ->
          Lwt.fail exn)

  let report_exn connection _flow = fun exn ->
    (* This needs to handle two cases. The case where the socket is
     * still open and we can gracefully respond with an error, and the
     * case where the client has already left. The second case is more
     * common when communicating over HTTPS, given that the remote peer
     * can close the connection without requiring an acknowledgement:
     *
     * From RFC5246§7.2.1:
     *   Unless some other fatal alert has been transmitted, each party
     *   is required to send a close_notify alert before closing the
     *   write side of the connection.  The other party MUST respond
     *   with a close_notify alert of its own and close down the
     *   connection immediately, discarding any pending writes. It is
     *   not required for the initiator of the close to wait for the
     *   responding close_notify alert before closing the read side of
     *   the connection. *)
    H2.Server_connection.report_exn connection exn;
    Lwt.return_unit
end

module Server = struct
  include H2_lwt.Server (Io)

  let create_connection_handler ?config ~request_handler ~error_handler =
    fun flow ->
      let request_handler = fun () -> request_handler in
      let error_handler = fun () -> error_handler in
      create_connection_handler ?config ~request_handler ~error_handler () flow
end

module type Server_intf = sig
  open H2

  val create_connection_handler
    :  ?config : Config.t
    -> request_handler : Server_connection.request_handler
    -> error_handler : Server_connection.error_handler
    -> (Conduit_mirage.Flow.flow -> unit Lwt.t)
end

module Server_with_conduit = struct
  open Conduit_mirage

  include Server

  type t = Conduit_mirage.Flow.flow -> unit Lwt.t

  let listen handler flow =
    Lwt.finalize
      (fun () -> handler flow)
      (fun () -> Flow.close flow)

  let connect t =
    let listen s f = Conduit_mirage.listen t s (listen f) in
    Lwt.return listen
end

(* module Client = Httpaf_lwt.Client (Io) *)
