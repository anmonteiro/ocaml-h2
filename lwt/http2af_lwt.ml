(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    Copyright (c) 2019 Antonio N. Monteiro.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Http2af
open Lwt.Infix

(* Based on the Buffer module in httpaf_async.ml. *)
module Buffer : sig
  type t

  val create : int -> t

  val get : t -> f:(Bigstringaf.t -> off:int -> len:int -> int) -> int

  val put
    :  t
    -> f:(Bigstringaf.t -> off:int -> len:int -> [ `Eof | `Ok of int ] Lwt.t)
    -> [ `Eof | `Ok of int ] Lwt.t
end = struct
  type t =
    { buffer      : Bigstringaf.t
    ; mutable off : int
    ; mutable len : int }

  let create size =
    let buffer = Bigstringaf.create size in
    { buffer; off = 0; len = 0 }

  let compress t =
    if t.len = 0
    then begin
      t.off <- 0;
      t.len <- 0;
    end else if t.off > 0 then begin
      Bigstringaf.blit t.buffer ~src_off:t.off t.buffer ~dst_off:0 ~len:t.len;
      t.off <- 0;
    end

  let get t ~f =
    let n = f t.buffer ~off:t.off ~len:t.len in
    t.off <- t.off + n;
    t.len <- t.len - n;
    if t.len = 0
    then t.off <- 0;
    n

  let put t ~f =
    compress t;
    (* XXX: dinosaure wants a comment here. *)
    assert (t.off = 0);
    f t.buffer
      ~off:(t.off + t.len)
      ~len:(Bigstringaf.length t.buffer - t.len (* - t.off *))
    >|= function
        | `Eof -> `Eof
        | `Ok n as ret ->
          t.len <- t.len + n;
          ret
end

module type IO = sig
  type socket
  type addr

  val read
    :  socket
    -> Bigstringaf.t
    -> off:int
    -> len:int
    -> [ `Eof | `Ok of int ] Lwt.t

  val writev
     : socket
    -> Faraday.bigstring Faraday.iovec list
    -> [ `Closed | `Ok of int ] Lwt.t

  val shutdown_send : socket -> unit

  val shutdown_receive : socket -> unit

  val close : socket -> unit Lwt.t

  val report_exn : Http2af.Server_connection.t -> socket -> exn -> unit Lwt.t
end

module Config = Http2af.Config

module Server  (Io: IO) = struct
  let start_read_write_loops
    ?(readf=Io.read)
    ?(writev=Io.writev)
    ~config
    ~socket
    connection =
    let report_exn = Io.report_exn connection socket in

    let read_buffer = Buffer.create config.Config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let rec read_loop () =
      let rec read_loop_step () =
        match Server_connection.next_read_operation connection with
        | `Read ->
          Buffer.put ~f:(readf socket) read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Server_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Yield ->
          Server_connection.yield_reader connection read_loop;
          Lwt.return_unit

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          Io.shutdown_receive socket;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          report_exn)
    in


    let writev = writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Server_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Server_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Server_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Io.shutdown_send socket;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          report_exn)
    in


    read_loop ();
    write_loop ();
    Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

    Io.close socket

  let create_connection_handler ?(config=Config.default) ~request_handler ~error_handler =
    fun client_addr socket ->
      let connection =
        Server_connection.create
          ~config
          ~error_handler:(error_handler client_addr)
          (request_handler client_addr)
      in
      start_read_write_loops ~config ~socket connection
end



(* module Client = struct
  module Client_connection = Http2af.Client_connection

  let start_read_write_loops
    ?(readf=read)
    ?(writev=Faraday_lwt_unix.writev_of_fd)
    ~config
    ~socket
    connection =
    let read_buffer = Buffer.create config.Config.read_buffer_size in
    let read_loop_exited, notify_read_loop_exited = Lwt.wait () in

    let read_loop () =
      let rec read_loop_step () =
        match Client_connection.next_read_operation connection with
        | `Read ->
          readf socket read_buffer >>= begin function
          | `Eof ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read_eof connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          | `Ok _ ->
            Buffer.get read_buffer ~f:(fun bigstring ~off ~len ->
              Client_connection.read connection bigstring ~off ~len)
            |> ignore;
            read_loop_step ()
          end

        | `Close ->
          Lwt.wakeup_later notify_read_loop_exited ();
          if not (Lwt_unix.state socket = Lwt_unix.Closed) then begin
            shutdown socket Unix.SHUTDOWN_RECEIVE
          end;
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          read_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    let writev = writev socket in
    let write_loop_exited, notify_write_loop_exited = Lwt.wait () in

    let rec write_loop () =
      let rec write_loop_step () =
        match Client_connection.next_write_operation connection with
        | `Write io_vectors ->
          writev io_vectors >>= fun result ->
          Client_connection.report_write_result connection result;
          write_loop_step ()

        | `Yield ->
          Client_connection.yield_writer connection write_loop;
          Lwt.return_unit

        | `Close _ ->
          Lwt.wakeup_later notify_write_loop_exited ();
          Lwt.return_unit
      in

      Lwt.async (fun () ->
        Lwt.catch
          write_loop_step
          (fun exn ->
            Client_connection.report_exn connection exn;
            Lwt.return_unit))
    in


    read_loop ();
    write_loop ();

    Lwt.async (fun () ->
      Lwt.join [read_loop_exited; write_loop_exited] >>= fun () ->

      if Lwt_unix.state socket <> Lwt_unix.Closed then
        Lwt.catch
          (fun () -> Lwt_unix.close socket)
          (fun _exn -> Lwt.return_unit)
      else
        Lwt.return_unit)

  let request ?(config=Config.default) socket request ~error_handler ~response_handler =
    let request_body, connection =
      Client_connection.request ~config request ~error_handler ~response_handler
    in

    start_read_write_loops ~config ~socket connection;
    request_body

  module TLS = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Tls_io.make_client ?client socket >|= fun tls_client ->
        let readf = Tls_io.readf tls_client in
        let writev = Tls_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end

  module SSL = struct
    let request ?client ?(config=Config.default) socket request ~error_handler ~response_handler =
      let request_body, connection =
        Client_connection.request ~config request ~error_handler ~response_handler
      in

      Lwt.async(fun () ->
        Ssl_io.make_client ?client socket >|= fun tls_client ->
        let readf = Ssl_io.readf tls_client in
        let writev = Ssl_io.writev tls_client in

        start_read_write_loops ~config ~readf ~writev ~socket connection);
      request_body
  end
end *)
