(*
 * This code was adapted from
 * https://github.com/mirage/mirage-www/blob/master/src/dispatch_tls.ml.
 *
 * Its copyright header is retained below.
 *
 * Copyright (c) 2015 Thomas Gazagnaire <thomas@gazagnaire.org>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt.Infix

module type HTTP = Httpaf_mirage.Server

module type HTTP2 = H2_mirage.Server

module Dispatch (Http : HTTP) (Https : HTTP) (Http2 : HTTP2) = struct
  let redirect =
    Http.create_connection_handler
      ?config:None
      ~request_handler:Http1_handler.redirect_handler
      ~error_handler:Http1_handler.redirect_error_handler

  let http1_handler =
    Https.create_connection_handler
      ?config:None
      ~request_handler:Http1_handler.request_handler
      ~error_handler:Http1_handler.error_handler

  let h2_handler =
    Http2.create_connection_handler
      ?config:None
      ~request_handler:H2_handler.request_handler
      ~error_handler:H2_handler.error_handler
end

module Make
    (Random : Mirage_random.S)
    (S : Mirage_stack.V4)
    (KEYS : Mirage_kv.RO)
    (C : Mirage_console.S)
    (Clock : Mirage_clock.PCLOCK) =
struct
  module X509 = Tls_mirage.X509 (KEYS) (Clock)
  module TCP = S.TCPV4
  module TLS = Tls_mirage.Make (TCP)
  module Http = Httpaf_mirage.Server (TCP)
  module Https = Httpaf_mirage.Server (TLS)
  module Http2 = H2_mirage.Server (TLS)
  module D = Dispatch (Http) (Https) (Http2)

  let log_src = Logs.Src.create "dispatch_tls" ~doc:"web-over-tls server"

  module Log = (val Logs.src_log log_src : Logs.LOG)

  let log c fmt = Printf.ksprintf (C.log c) fmt

  let with_tls cfg tcp ~f =
    let peer, port = TCP.dst tcp in
    let log str =
      Log.debug (fun f -> f "[%s:%d] %s" (Ipaddr.V4.to_string peer) port str)
    in
    TLS.server_of_flow cfg tcp >>= function
    | Error _ ->
      log "TLS failed";
      TCP.close tcp
    | Ok tls_server ->
      log "TLS ok";
      f tls_server >>= fun () -> TLS.close tls_server

  let tls_init kv =
    X509.certificate kv `Default >|= fun certificate ->
    Tls.Config.server
      ~alpn_protocols:[ "h2"; "http/1.1" ] (* accept h2 before http/1.1 *)
      ~certificates:(`Single certificate)
      ~ciphers:
        (List.filter
           Tls.Ciphersuite.ciphersuite_tls12_only
           Tls.Config.Ciphers.supported)
      ()

  let start _random stack keys c _clock =
    tls_init keys >>= fun tls_config ->
    log c "started unikernel listen (http: 8080, https: 9443)" >>= fun () ->
    S.listen_tcpv4 stack ~port:8080 D.redirect;
    S.listen_tcpv4 stack ~port:9443 (fun flow ->
        with_tls tls_config flow ~f:(fun flow ->
            match TLS.epoch flow with
            | Error () ->
              Lwt_io.eprintlf
                "Unable to fetch session data. Did the handshake fail?"
            | Ok { Tls.Core.alpn_protocol; _ } ->
              (match alpn_protocol with
              | None ->
                (* Unable to negotiate a protocol *)
                Lwt.return_unit
              | Some "http/1.1" ->
                D.http1_handler flow
              | Some "h2" ->
                D.h2_handler flow
              | _ ->
                (* Can't really happen - would mean that TLS negotiated a
                 * protocol that we didn't specify. *)
                assert false)));
    S.listen stack >>= fun () ->
    let forever, _ = Lwt.wait () in
    forever
end
