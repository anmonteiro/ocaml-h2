(*----------------------------------------------------------------------------
    Copyright (c) 2019 António Nuno Monteiro

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are met:

    1. Redistributions of source code must retain the above copyright notice,
    this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.

    3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from this
    software without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
  POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Lwt.Infix

let () = Ssl.init ()

module Io : H2_lwt.IO with
    type socket = Lwt_ssl.socket
    and type addr = Unix.sockaddr = struct
  type socket = Lwt_ssl.socket
  type addr = Unix.sockaddr

  let read ssl bigstring ~off ~len =
    Lwt.catch
      (fun () ->
        (* Lwt_unix.blocking (Lwt_ssl.get_fd socket) >>= fun _ -> *)
        Lwt_ssl.read_bytes ssl bigstring off len)
      (function
      | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
        Lwt.fail exn
      | exn ->
        Lwt.async (fun () ->
          Lwt_ssl.ssl_shutdown ssl >>= fun () ->
          Lwt_ssl.close ssl);
        Lwt.fail exn)
    >>= fun bytes_read ->
      if bytes_read = 0 then
        Lwt.return `Eof
      else
        Lwt.return (`Ok bytes_read)

  let writev ssl = fun iovecs ->
    Lwt.catch
      (fun () ->
        Lwt_list.fold_left_s (fun acc {Faraday.buffer; off; len} ->
          Lwt_ssl.write_bytes ssl buffer off len
          >|= fun written -> acc + written) 0 iovecs
        >|= fun n -> `Ok n)
      (function
      | Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
        Lwt.return `Closed
      | exn ->
        Lwt.fail exn)

  let shutdown_send ssl =
    ignore (Lwt_ssl.ssl_shutdown ssl >|= fun () ->
      Lwt_ssl.shutdown ssl Unix.SHUTDOWN_SEND)

  let shutdown_receive ssl =
    ignore (Lwt_ssl.ssl_shutdown ssl >|= fun () ->
      Lwt_ssl.shutdown ssl Unix.SHUTDOWN_RECEIVE)

  let close = Lwt_ssl.close

  let report_exn connection ssl = fun exn ->
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
    begin match Lwt_unix.state (Lwt_ssl.get_fd ssl) with
    | Aborted _
    | Closed ->
      H2.Server_connection.shutdown connection
    | Opened ->
      H2.Server_connection.report_exn connection exn
    end;
    Lwt.return_unit
end

type client = Lwt_ssl.socket
type server = Lwt_ssl.socket

let make_client ?client socket =
  match client with
  | Some client -> Lwt.return client
  | None ->
    let client_ctx = Ssl.create_context Ssl.SSLv23 Ssl.Client_context in
    Ssl.disable_protocols client_ctx [Ssl.SSLv23];
    Ssl.set_context_alpn_protos client_ctx ["h2"];
    Ssl.honor_cipher_order client_ctx;
    Lwt_ssl.ssl_connect socket client_ctx

(* TODO: this needs error handling or it'll crash the server *)
let make_server ?server ?certfile ?keyfile socket =
  match server, certfile, keyfile with
  | Some server, _, _ -> Lwt.return server
  | None, Some cert, Some priv_key ->
    let server_ctx = Ssl.create_context Ssl.TLSv1_3 Ssl.Server_context in
    Ssl.disable_protocols server_ctx [Ssl.SSLv23];
    Ssl.use_certificate server_ctx cert priv_key;
    (* let rec first_match l1 = function
    | [] -> None
    | x::_ when List.mem x l1 -> Some x
    | _::xs -> first_match l1 xs
    in *)
    Ssl.set_context_alpn_protos server_ctx ["h2"];
    (* Ssl.set_context_alpn_select_callback server_ctx (fun client_protos ->
      first_match client_protos ["h2"]
    ); *)
    Lwt_ssl.ssl_accept socket server_ctx
  | _ ->
    Lwt.fail (Invalid_argument "Certfile and Keyfile required when server isn't provided")

