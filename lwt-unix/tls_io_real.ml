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

open Lwt.Infix

type descriptor = Tls_lwt.Unix.t

module Io :
  H2_lwt.IO with type socket = descriptor and type addr = Unix.sockaddr = struct
  type socket = descriptor

  type addr = Unix.sockaddr

  let read tls bigstring ~off ~len =
    Lwt.catch
      (fun () -> Tls_lwt.Unix.read_bytes tls bigstring off len)
      (function
        | Unix.Unix_error (Unix.EBADF, _, _) as exn ->
          Lwt.fail exn
        | exn ->
          Lwt.async (fun () -> Tls_lwt.Unix.close tls);
          Lwt.fail exn)
    >>= fun bytes_read ->
    if bytes_read = 0 then
      Lwt.return `Eof
    else
      Lwt.return (`Ok bytes_read)

  let writev tls iovecs =
    Lwt.catch
      (fun () ->
        let cstruct_iovecs =
          List.map
            (fun { Faraday.len; buffer; off } ->
              Cstruct.of_bigarray ~off ~len buffer)
            iovecs
        in
        Tls_lwt.Unix.writev tls cstruct_iovecs >|= fun () ->
        `Ok (Cstruct.lenv cstruct_iovecs))
      (function
        | Unix.Unix_error (Unix.EBADF, "check_descriptor", _) ->
          Lwt.return `Closed
        | exn ->
          Lwt.fail exn)

  let shutdown_send tls = ignore (Tls_lwt.Unix.close_tls tls)

  let shutdown_receive tls = ignore (Tls_lwt.Unix.close_tls tls)

  let close = Tls_lwt.Unix.close

  let state tls =
    match Tls_lwt.Unix.epoch tls with `Error -> `Error | `Ok _ -> `Open
end

let null_auth ~host:_ _ = Ok None

let make_client socket =
  let config =
    Tls.Config.client ~authenticator:null_auth ~alpn_protocols:[ "h2" ] ()
  in
  Tls_lwt.Unix.client_of_fd config socket

(* This function does not perform error handling and will therefore crash a
 * server in case e.g. the handshake fails. *)
let make_server ~certfile ~keyfile socket =
  X509_lwt.private_of_pems ~cert:certfile ~priv_key:keyfile
  >>= fun certificate ->
  let config =
    Tls.Config.server
      ~alpn_protocols:[ "h2" ]
      ~certificates:(`Single certificate)
      ~ciphers:
        (List.filter
           Tls.Ciphersuite.ciphersuite_tls12_only
           Tls.Config.Ciphers.supported)
      ()
  in
  Tls_lwt.Unix.server_of_fd config socket
