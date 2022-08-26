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

module Server : sig
  include H2_eio_intf.Server with type socket = Gluten_eio.Server.socket

  module SSL : sig
    include H2_eio_intf.Server with type socket = Gluten_eio.Server.SSL.socket

    val create_connection_handler_with_default
      :  certfile:string
      -> keyfile:string
      -> ?config:H2.Config.t
      -> request_handler:(Eio.Net.Sockaddr.stream -> H2.Reqd.t -> unit)
      -> error_handler:
           (Eio.Net.Sockaddr.stream -> H2.Server_connection.error_handler)
      -> Eio.Net.Sockaddr.stream
      -> Eio.Net.stream_socket
      -> unit
  end
end

module type Client = H2_eio_intf.Client
module type Server = H2_eio_intf.Server

module Client : sig
  include
    H2_eio_intf.Client
      with type socket = Gluten_eio.Client.socket
       and type runtime = Gluten_eio.Client.t

  module SSL : sig
    include
      H2_eio_intf.Client
        with type socket = Gluten_eio.Client.SSL.socket
         and type runtime = Gluten_eio.Client.SSL.t

    val create_connection_with_default
      :  ?config:H2.Config.t
      -> ?push_handler:
           (H2.Request.t
            -> (H2.Client_connection.response_handler, unit) result)
      -> sw:Eio.Switch.t
      -> error_handler:H2.Client_connection.error_handler
      -> Eio.Net.stream_socket
      -> t
  end
end
