(*----------------------------------------------------------------------------
 *  Copyright (c) 2022 António Nuno Monteiro
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

module MakeServer (Server_runtime : Gluten_eio.Server.S) = struct
  type socket = Server_runtime.socket

  let create_connection_handler
      ?(config = H2.Config.default)
      ~domain_mgr
      ~request_handler
      ~error_handler
      client_addr
      socket
    =
    let connection =
      H2.Server_connection.create
        ~config
        ~error_handler:(error_handler client_addr)
        (request_handler client_addr)
    in
    Server_runtime.create_connection_handler
      ~domain_mgr
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module H2.Server_connection)
      connection
      client_addr
      socket
end

module MakeClient (Client_runtime : Gluten_eio.Client.S) = struct
  type socket = Client_runtime.socket
  type runtime = Client_runtime.t

  type t =
    { connection : H2.Client_connection.t
    ; runtime : runtime
    }

  let create_connection
      ?(config = H2.Config.default)
      ?push_handler
      ~domain_mgr
      ~error_handler
      socket
    =
    let connection =
      H2.Client_connection.create ~config ?push_handler ~error_handler
    in
    let runtime =
      Client_runtime.create
        ~read_buffer_size:config.read_buffer_size
        ~domain_mgr
        ~protocol:(module H2.Client_connection)
        connection
        socket
    in
    { runtime; connection }

  let request t = H2.Client_connection.request t.connection
  let ping t = H2.Client_connection.ping t.connection
  let shutdown t = Client_runtime.shutdown t.runtime
  let is_closed t = Client_runtime.is_closed t.runtime
end

module Server = struct
  include MakeServer (Gluten_eio.Server)

  module SSL = struct
    include MakeServer (Gluten_eio.Server.SSL)

    let create_connection_handler_with_default
        ~certfile
        ~keyfile
        ?config
        ~domain_mgr
        ~request_handler
        ~error_handler
      =
      let make_ssl_server =
        Gluten_eio.Server.SSL.create_default
          ~alpn_protocols:[ "h2" ]
          ~certfile
          ~keyfile
      in
      fun client_addr socket ->
        let ssl_server = make_ssl_server client_addr socket in
        create_connection_handler
          ?config
          ~domain_mgr
          ~request_handler
          ~error_handler
          client_addr
          ssl_server
  end
end

module Client = struct
  include MakeClient (Gluten_eio.Client)

  module SSL = struct
    include MakeClient (Gluten_eio.Client.SSL)

    let create_connection_with_default
        ?config
        ?push_handler
        ~domain_mgr
        ~error_handler
        socket
      =
      let ssl_client =
        Gluten_eio.Client.SSL.create_default ~alpn_protocols:[ "h2" ] socket
      in
      create_connection
        ?config
        ?push_handler
        ~domain_mgr
        ~error_handler
        ssl_client
  end
end
