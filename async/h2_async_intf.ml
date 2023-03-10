(*----------------------------------------------------------------------------
 *  Copyright (c) 2019-2020 António Nuno Monteiro
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

open Async
open H2

module type Server = sig
  type 'a socket constraint 'a = [< Socket.Address.t ]

  val create_connection_handler :
     ?config:Config.t
    -> request_handler:('a -> Server_connection.request_handler)
    -> error_handler:('a -> Server_connection.error_handler)
    -> 'a
    -> 'a socket
    -> unit Deferred.t
end

module type Client = sig
  type 'a socket constraint 'a = [< Socket.Address.t ]
  type 'a runtime constraint 'a = [< Socket.Address.t ]

  type 'a t =
    { connection : Client_connection.t
    ; runtime : 'a runtime
    }

  val create_connection :
     ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> error_handler:Client_connection.error_handler
    -> 'a socket
    -> 'a t Deferred.t

  val request :
     'a t
    -> ?flush_headers_immediately:bool
    -> ?trailers_handler:Client_connection.trailers_handler
    -> Request.t
    -> error_handler:Client_connection.error_handler
    -> response_handler:Client_connection.response_handler
    -> Body.Writer.t

  val ping :
     _ t
    -> ?payload:Bigstringaf.t
    -> ?off:int
    -> ((unit, [ `EOF ]) result -> unit)
    -> unit

  val shutdown : _ t -> unit Deferred.t
  val is_closed : _ t -> bool
end
