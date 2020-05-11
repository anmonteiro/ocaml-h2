(*----------------------------------------------------------------------------
 *  Copyright (c) 2018 Inhabited Type LLC.
 *  Copyright (c) 2019-2020 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

open Async
open H2

module Server : sig
  include
    H2_async_intf.Server
      with type socket := ([ `Active ], Socket.Address.Inet.t) Socket.t
       and type addr := Socket.Address.Inet.t

  module SSL : sig
    include
      H2_async_intf.Server
        with type socket := Gluten_async.Server.SSL.socket
         and type addr := Socket.Address.Inet.t

    val create_connection_handler_with_default
      :  certfile:string
      -> keyfile:string
      -> ?config:Config.t
      -> request_handler:('a -> Server_connection.request_handler)
      -> error_handler:('a -> Server_connection.error_handler)
      -> (Socket.Address.Inet.t as 'a)
      -> ([ `Active ], 'a) Socket.t
      -> unit Deferred.t
  end
end

module Client : sig
  include
    H2_async_intf.Client
      with type socket = ([ `Active ], Socket.Address.Inet.t) Socket.t

  module SSL : sig
    include
      H2_async_intf.Client with type socket = Gluten_async.Client.SSL.socket

    val create_connection_with_default
      :  ?config:Config.t
      -> ?push_handler:
           (Request.t -> (Client_connection.response_handler, unit) result)
      -> error_handler:Client_connection.error_handler
      -> ([ `Active ], [< Socket.Address.t ]) Socket.t
      -> t Deferred.t
  end
end
