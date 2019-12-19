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

open Async
open H2

module type IO = sig
  type socket

  type addr

  val read
    :  socket
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> [ `Eof | `Ok of int ] Deferred.t
  (** The region [[off, off + len)] is where read bytes can be written to *)

  val writev
    :  socket
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Deferred.t

  val shutdown_send : socket -> unit

  val shutdown_receive : socket -> unit

  val close : socket -> unit Deferred.t

  val state : socket -> [ `Open | `Error | `Closed ]
end

module type Server = sig
  type socket

  type addr

  val create_connection_handler
    :  ?config:Config.t
    -> request_handler:(addr -> Server_connection.request_handler)
    -> error_handler:(addr -> Server_connection.error_handler)
    -> addr
    -> socket
    -> unit Deferred.t
end

module type Client = sig
  type t

  type socket

  val create_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> error_handler:Client_connection.error_handler
    -> socket
    -> t Deferred.t

  (* From RFC7540§3.1:
   *   The string "h2c" identifies the protocol where HfTTP/2 is run over
   *   cleartext TCP. This identifier is used in the HTTP/1.1 Upgrade header
   *   field and in any place where HTTP/2 over TCP is identified. *)
  val create_h2c_connection
    :  ?config:Config.t
    -> ?push_handler:
         (Request.t -> (Client_connection.response_handler, unit) result)
    -> http_request:Httpaf.Request.t
    -> error_handler:Client_connection.error_handler
    -> Client_connection.response_handler * Client_connection.error_handler
    -> socket
    -> (t, string) Deferred.Result.t

  val request
    :  t
    -> Request.t
    -> error_handler:Client_connection.error_handler
    -> response_handler:Client_connection.response_handler
    -> [ `write ] Body.t

  val ping : t -> ?payload:Bigstringaf.t -> ?off:int -> (unit -> unit) -> unit

  val shutdown : t -> unit

  val is_closed : t -> bool
end
