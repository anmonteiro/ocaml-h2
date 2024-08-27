(*----------------------------------------------------------------------------
 *  Copyright (c) 2017 Inhabited Type LLC.
 *  Copyright (c) 2019 Antonio N. Monteiro.
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

module Reader = struct
  type t =
    { faraday : Faraday.t
    ; mutable read_scheduled : bool
    ; mutable on_eof : unit -> unit
    ; mutable on_read : Bigstringaf.t -> off:int -> len:int -> unit
    ; buffered_bytes : int ref
    ; done_reading : int -> unit
    }

  let default_done_reading = Sys.opaque_identity (fun _ -> ())
  let default_on_eof = Sys.opaque_identity (fun () -> ())
  let default_on_read = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> ())

  let create buffer ~done_reading =
    { faraday = Faraday.of_bigstring buffer
    ; read_scheduled = false
    ; on_eof = default_on_eof
    ; on_read = default_on_read
    ; buffered_bytes = ref 0
    ; done_reading
    }

  let create_empty () =
    let t = create Bigstringaf.empty ~done_reading:default_done_reading in
    Faraday.close t.faraday;
    t

  let empty = create_empty ()
  let is_closed t = Faraday.is_closed t.faraday
  let unsafe_faraday t = t.faraday

  let rec do_execute_read t on_eof on_read =
    match Faraday.operation t.faraday with
    | `Yield -> ()
    | `Close ->
      t.read_scheduled <- false;
      t.on_eof <- default_on_eof;
      t.on_read <- default_on_read;
      on_eof ()
    | `Writev [] -> assert false
    | `Writev (iovec :: _) ->
      t.read_scheduled <- false;
      t.on_eof <- default_on_eof;
      t.on_read <- default_on_read;
      let { Httpun_types.IOVec.buffer; off; len } = iovec in
      Faraday.shift t.faraday len;
      on_read buffer ~off ~len;
      (* Application is done reading, we can give flow control tokens back to
         the peer. *)
      t.done_reading len;
      execute_read t

  and execute_read t =
    if t.read_scheduled then do_execute_read t t.on_eof t.on_read

  let schedule_read t ~on_eof ~on_read =
    if t.read_scheduled
    then failwith "Body.schedule_read: reader already scheduled";
    if not (is_closed t)
    then (
      t.read_scheduled <- true;
      t.on_eof <- on_eof;
      t.on_read <- on_read);
    do_execute_read t on_eof on_read

  let close t =
    Faraday.close t.faraday;
    execute_read t

  let has_pending_output t = Faraday.has_pending_output t.faraday
end

module Writer = struct
  module Writer = Serialize.Writer

  type t =
    { faraday : Faraday.t
    ; buffered_bytes : int ref
    ; writer : Serialize.Writer.t
    }

  let create buffer ~writer =
    { faraday = Faraday.of_bigstring buffer; buffered_bytes = ref 0; writer }

  let create_empty ~writer =
    let t = create Bigstringaf.empty ~writer in
    Faraday.close t.faraday;
    t

  let ready_to_write t = Serialize.Writer.wakeup t.writer

  let write_char t c =
    if not (Faraday.is_closed t.faraday) then Faraday.write_char t.faraday c;
    ready_to_write t

  let write_string t ?off ?len s =
    if not (Faraday.is_closed t.faraday)
    then Faraday.write_string ?off ?len t.faraday s;
    ready_to_write t

  let write_bigstring t ?off ?len b =
    if not (Faraday.is_closed t.faraday)
    then Faraday.write_bigstring ?off ?len t.faraday b;
    ready_to_write t

  let schedule_bigstring t ?off ?len (b : Bigstringaf.t) =
    if not (Faraday.is_closed t.faraday)
    then Faraday.schedule_bigstring ?off ?len t.faraday b;
    ready_to_write t

  let flush t kontinue =
    if Serialize.Writer.is_closed t.writer
    then kontinue `Closed
    else (
      Faraday.flush_with_reason t.faraday (function
        | Drain -> kontinue `Closed
        | Nothing_pending | Shift -> kontinue `Written);
      ready_to_write t)

  let is_closed t = Faraday.is_closed t.faraday
  let has_pending_output t = Faraday.has_pending_output t.faraday

  let close_and_drain t =
    Faraday.close t.faraday;
    (* Resolve all pending flushes *)
    ignore (Faraday.drain t.faraday : int)

  let close t =
    Serialize.Writer.unyield t.writer;
    Faraday.close t.faraday;
    ready_to_write t

  let unsafe_faraday t = t.faraday

  let transfer_to_writer t writer ~max_frame_size ~max_bytes stream_id =
    let faraday = t.faraday in
    if Serialize.Writer.is_closed t.writer
    then (
      close_and_drain t;
      0)
    else
      match Faraday.operation faraday with
      | `Yield | `Close -> 0
      | `Writev iovecs ->
        let buffered = t.buffered_bytes in
        let iovecs = Httpun_types.IOVec.shiftv iovecs !buffered in
        let lengthv = Httpun_types.IOVec.lengthv iovecs in
        let writev_len = if max_bytes < lengthv then max_bytes else lengthv in
        buffered := !buffered + writev_len;
        let frame_info = Writer.make_frame_info ~max_frame_size stream_id in
        Writer.schedule_iovecs writer frame_info ~len:writev_len iovecs;
        Writer.flush t.writer (function
          | `Closed -> close_and_drain t
          | `Written ->
            Faraday.shift faraday writev_len;
            buffered := !buffered - writev_len);
        writev_len
end
