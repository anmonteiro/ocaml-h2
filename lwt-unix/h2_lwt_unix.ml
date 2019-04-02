(*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open H2
open Lwt.Infix

module Io : H2_lwt.IO with
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
    begin match Lwt_unix.state socket with
    | Aborted _
    | Closed ->
      Server_connection.shutdown connection
    | Opened ->
      Server_connection.report_exn connection exn
    end;
    Lwt.return_unit
end

module Config = H2.Config

module Server = struct
  include H2_lwt.Server (Io)

  module TLS = struct
    include H2_lwt.Server (Tls_io.Io)

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
    include H2_lwt.Server (Ssl_io.Io)
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



module Client = struct
  include H2_lwt.Client (Io)

  module TLS = struct
    include H2_lwt.Client (Tls_io.Io)

    let create_connection ?client ?(config=Config.default) ~error_handler socket =
      Tls_io.make_client ?client socket >>= fun tls_client ->
      create_connection
        ~config
        ~error_handler
        (socket, tls_client)
  end

  module SSL = struct
    include H2_lwt.Client (Ssl_io.Io)

    let create_connection ?client ?(config=Config.default) ~error_handler socket =
      Ssl_io.make_client ?client socket >>= fun ssl_client ->
      create_connection
        ~config
        ~error_handler
        ssl_client
  end
end

