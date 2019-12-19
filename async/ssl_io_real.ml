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

open Core
open Async
open Async_ssl
module Unix = Core.Unix

type descriptor = Ssl.Connection.t * Reader.t * Writer.t

module Io :
  H2_async_intf.IO
    with type socket = descriptor
     and type addr =
          [ Socket.Address.Inet.t
          | Socket.Address.Unix.t
          ] = struct
  type socket = descriptor

  type addr =
    [ Socket.Address.Inet.t
    | Socket.Address.Unix.t
    ]

  let read (_conn, reader, _writer) bigstring ~off ~len =
    let bigsubstr = Bigsubstring.create ~pos:off ~len bigstring in
    Reader.read_bigsubstring reader bigsubstr

  let writev (_conn, _reader, writer) iovecs =
    let iovecs_q = Queue.create ~capacity:(List.length iovecs) () in
    let len =
      List.fold
        ~init:0
        ~f:(fun acc { Faraday.buffer; off = pos; len } ->
          Queue.enqueue iovecs_q (Unix.IOVec.of_bigstring ~pos ~len buffer);
          acc + len)
        iovecs
    in
    if Writer.is_closed writer then
      Deferred.return `Closed
    else (
      Writer.schedule_iovecs writer iovecs_q;
      Writer.flushed writer >>| fun () -> `Ok len)

  let shutdown_send (_conn, _reader, writer) =
    Async.don't_wait_for (Writer.close writer)

  let shutdown_receive (_conn, reader, _writer) =
    Async.don't_wait_for (Reader.close reader)

  let close (conn, _reader, _writer) =
    Ssl.Connection.close conn;
    Deferred.unit

  let state (_conn, reader, writer) =
    if Writer.is_stopped_permanently writer then
      `Error
    else if Writer.is_closed writer && Reader.is_closed reader then
      `Closed
    else
      `Open
end

(* taken from https://github.com/janestreet/async_extra/blob/master/src/tcp.ml *)
let reader_writer_of_sock
    ?buffer_age_limit ?reader_buffer_size ?writer_buffer_size s
  =
  let fd = Socket.fd s in
  ( Reader.create ?buf_len:reader_buffer_size fd
  , Writer.create ?buffer_age_limit ?buf_len:writer_buffer_size fd )

let connect r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.client
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ~verify_modes:[ Ssl.Verify_mode.Verify_none ]
    ()
  |> Deferred.Or_error.ok_exn
  >>= fun conn ->
  Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd
  >>= fun app_reader ->
  Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr
  >>| fun (app_writer, _) ->
  don't_wait_for
    ( Deferred.all_unit
        [ Writer.close_finished app_writer; Reader.close_finished app_reader ]
    >>= fun () ->
      Ssl.Connection.close conn;
      Pipe.close_read app_rd;
      Writer.close w );
  conn, app_reader, app_writer

let make_default_client socket =
  let reader, writer = reader_writer_of_sock socket in
  connect reader writer

let listen ~crt_file ~key_file r w =
  let net_to_ssl = Reader.pipe r in
  let ssl_to_net = Writer.pipe w in
  let app_to_ssl, app_wr = Pipe.create () in
  let app_rd, ssl_to_app = Pipe.create () in
  Ssl.server
    ~crt_file
    ~key_file
    ~app_to_ssl
    ~ssl_to_app
    ~net_to_ssl
    ~ssl_to_net
    ()
  |> Deferred.Or_error.ok_exn
  >>= fun conn ->
  Reader.of_pipe (Info.of_string "httpaf_async_ssl_reader") app_rd
  >>= fun app_reader ->
  Writer.of_pipe (Info.of_string "httpaf_async_ssl_writer") app_wr
  >>| fun (app_writer, _) ->
  don't_wait_for
    ( Deferred.all_unit
        [ Reader.close_finished app_reader; Writer.close_finished app_writer ]
    >>= fun () ->
      Ssl.Connection.close conn;
      Pipe.close_read app_rd;
      Writer.close w );
  conn, app_reader, app_writer

let make_server ~certfile ~keyfile socket =
  let reader, writer = reader_writer_of_sock socket in
  listen ~crt_file:certfile ~key_file:keyfile reader writer
