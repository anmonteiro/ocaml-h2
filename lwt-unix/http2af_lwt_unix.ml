open Http2af
open Lwt.Infix

module Io : Http2af_lwt.IO with
    type socket = Lwt_unix.file_descr
    and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr
  type addr = Unix.sockaddr

  let shutdown socket command =
    try Lwt_unix.shutdown socket command
    with Unix.Unix_error (Unix.ENOTCONN, _, _) -> ()

  let shutdown_send socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_SEND

  let shutdown_receive socket =
    if not (Lwt_unix.state socket = Lwt_unix.Closed) then
      shutdown socket Unix.SHUTDOWN_RECEIVE

  let close socket =
    if Lwt_unix.state socket <> Lwt_unix.Closed then
      Lwt.catch
        (fun () -> Lwt_unix.close socket)
        (fun _exn -> Lwt.return_unit)
    else
      Lwt.return_unit

  let read fd bigstring ~off ~len =
    Lwt.catch
      (fun () -> Lwt_bytes.read fd bigstring off len)
      (function
      | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
      | exn ->
        Lwt.async (fun () ->
          Lwt_unix.close fd);
        Lwt.fail exn)

    >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev = Faraday_lwt_unix.writev_of_fd

  let report_exn connection socket = fun exn ->
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
    Printf.eprintf "EXN SOMETHING: %B %s %s\n%!"
      (Lwt_unix.state socket == Lwt_unix.Closed)
      (Printexc.to_string exn)
      (Printexc.get_backtrace ());

    begin match Lwt_unix.state socket with
    | Aborted _
    | Closed ->
      Server_connection.shutdown connection
    | Opened ->
      Server_connection.report_exn connection exn
    end;
    Lwt.return_unit
end

module Config = Http2af.Config

module Server = struct
  include Http2af_lwt.Server (Io)

  module TLS = struct
    include Http2af_lwt.Server (Tls_io.Io)

    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      fun client_addr socket ->
        Tls_io.make_server ?server ?certfile ?keyfile socket >>= fun tls_server ->
        create_connection_handler
          ~config
          ~request_handler
          ~error_handler
          client_addr
          (socket, tls_server)
  end

  module SSL = struct
    include Http2af_lwt.Server (Ssl_io.Io)
    let create_connection_handler
      ?server
      ?certfile
      ?keyfile
      ?(config=Config.default)
      ~request_handler
      ~error_handler =
      fun client_addr socket ->
        Ssl_io.make_server ?server ?certfile ?keyfile socket >>= fun ssl_server ->
        create_connection_handler
          ~config
          ~request_handler
          ~error_handler
          client_addr
          ssl_server
  end
end



(* module Client = struct
  module Client_connection = Http2af.Client_connection

  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_lwt_unix.writev_of_fd)
    ~config
    ~socket
    connection =
    let read_buffer = Buffer.create config.Config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          readf socket read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.async (fun () ->
      Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

      if Lwt_unix.state socket <> Lwt_unix.Closed then
        Lwt.catch
          (fun () -> Lwt_unix.close socket)
          (fun _exn -> Lwt.return_unit)
      else
        Lwt.return_unit)

  let request ?(config=Config.default) socket request ~error_handler ~response_handler =
    let request_body, connection =
      Client_connection.request ~config request ~error_handler ~response_handler
    in

    start_read_write_loops ~config ~socket connection;
    request_body

  module TLS = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Tls_io.make_client ?client socket >|= fun tls_client ->
        let readf = Tls_io.readf tls_client in
        let writev = Tls_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end

  module SSL = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Ssl_io.make_client ?client socket >|= fun tls_client ->
        let readf = Ssl_io.readf tls_client in
        let writev = Ssl_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end
end *)
