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
  val create_connection_handler :
     ?config:H2.Config.t
    -> request_handler:(Eio.Net.Sockaddr.stream -> H2.Reqd.t -> unit)
    -> error_handler:
         (Eio.Net.Sockaddr.stream -> H2.Server_connection.error_handler)
    -> Eio.Net.Sockaddr.stream
    -> Eio.Flow.two_way
    -> unit
end

module Client : sig
  type t =
    { connection : H2.Client_connection.t
    ; runtime : Gluten_eio.Client.t
    }

  val create_connection :
     ?config:H2.Config.t
    -> ?push_handler:
         (H2.Request.t -> (H2.Client_connection.response_handler, unit) result)
    -> sw:Eio.Switch.t
    -> error_handler:H2.Client_connection.error_handler
    -> Eio.Flow.two_way
    -> t

  val request :
     t
    -> ?flush_headers_immediately:bool
    -> ?trailers_handler:H2.Client_connection.trailers_handler
    -> H2.Request.t
    -> error_handler:H2.Client_connection.error_handler
    -> response_handler:H2.Client_connection.response_handler
    -> H2.Body.Writer.t

  val ping :
     ?payload:Bigstringaf.t
    -> ?off:int
    -> t
    -> (unit, [ `EOF ]) result Eio.Promise.t

  val shutdown : t -> unit Eio.Promise.t
  val is_closed : t -> bool
end
