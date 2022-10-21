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

module Server = struct
  let create_connection_handler
      ?(config = H2.Config.default)
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
    Gluten_eio.Server.create_connection_handler
      ~read_buffer_size:config.read_buffer_size
      ~protocol:(module H2.Server_connection)
      connection
      client_addr
      socket
end

module Client = struct
  type t =
    { connection : H2.Client_connection.t
    ; runtime : Gluten_eio.Client.t
    }

  let create_connection
      ?(config = H2.Config.default)
      ?push_handler
      ~sw
      ~error_handler
      socket
    =
    let connection =
      H2.Client_connection.create ~config ?push_handler ~error_handler ()
    in
    let runtime =
      Gluten_eio.Client.create
        ~read_buffer_size:config.read_buffer_size
        ~sw
        ~protocol:(module H2.Client_connection)
        connection
        socket
    in
    { runtime; connection }

  let request t = H2.Client_connection.request t.connection

  let ping ?payload ?off t =
    let p, u = Eio.Promise.create () in
    H2.Client_connection.ping ?payload ?off t.connection (Eio.Promise.resolve u);
    p

  let shutdown t = Gluten_eio.Client.shutdown t.runtime
  let is_closed t = Gluten_eio.Client.is_closed t.runtime
end
