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
module Config = H2.Config

module Server = struct
  include H2_lwt.Server (Gluten_lwt_unix.Server)

  module TLS = struct
    include H2_lwt.Server (Gluten_lwt_unix.Server.TLS)

    let create_connection_handler_with_default
        ~certfile ~keyfile ?config ~request_handler ~error_handler
      =
      let make_tls_server =
        Gluten_lwt_unix.Server.TLS.create_default
          ~alpn_protocols:[ "h2" ]
          ~certfile
          ~keyfile
      in
      fun client_addr socket ->
        make_tls_server client_addr socket >>= fun tls_server ->
        create_connection_handler
          ?config
          ~request_handler
          ~error_handler
          client_addr
          tls_server
  end

  module SSL = struct
    include H2_lwt.Server (Gluten_lwt_unix.Server.SSL)

    let create_connection_handler_with_default
        ~certfile ~keyfile ?config ~request_handler ~error_handler
      =
      let make_ssl_server =
        Gluten_lwt_unix.Server.SSL.create_default
          ~alpn_protocols:[ "h2" ]
          ~certfile
          ~keyfile
      in
      fun client_addr socket ->
        make_ssl_server client_addr socket >>= fun ssl_server ->
        create_connection_handler
          ?config
          ~request_handler
          ~error_handler
          client_addr
          ssl_server
  end
end

module Client = struct
  include H2_lwt.Client (Gluten_lwt_unix.Client)

  module TLS = struct
    include H2_lwt.Client (Gluten_lwt_unix.Client.TLS)

    let create_connection_with_default
        ?config ?push_handler ~error_handler socket
      =
      Gluten_lwt_unix.Client.TLS.create_default ~alpn_protocols:[ "h2" ] socket
      >>= fun tls_client ->
      create_connection ?config ?push_handler ~error_handler tls_client
  end

  module SSL = struct
    include H2_lwt.Client (Gluten_lwt_unix.Client.SSL)

    let create_connection_with_default
        ?config ?push_handler ~error_handler socket
      =
      Gluten_lwt_unix.Client.SSL.create_default ~alpn_protocols:[ "h2" ] socket
      >>= fun ssl_client ->
      create_connection ?config ?push_handler ~error_handler ssl_client
  end
end
