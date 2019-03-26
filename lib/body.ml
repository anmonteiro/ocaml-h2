type _ t =
  { faraday                        : Faraday.t
  ; mutable read_scheduled         : bool
  ; mutable write_final_data_frame : bool
  ; mutable on_eof                 : unit -> unit
  ; mutable on_read                : Bigstringaf.t -> off:int -> len:int -> unit
  ; mutable when_ready_to_write    : unit -> unit
  ; buffered_bytes                 : int ref
  }

let default_on_eof         = Sys.opaque_identity (fun () -> ())
let default_on_read        = Sys.opaque_identity (fun _ ~off:_ ~len:_ -> ())
let default_ready_to_write = Sys.opaque_identity (fun () -> ())

let of_faraday faraday =
  { faraday
  ; read_scheduled         = false
  ; write_final_data_frame = true
  ; on_eof                 = default_on_eof
  ; on_read                = default_on_read
  ; when_ready_to_write    = default_ready_to_write
  ; buffered_bytes         = ref 0
  }

let create buffer =
  of_faraday (Faraday.of_bigstring buffer)

let create_empty () =
  let t = create Bigstringaf.empty in
  Faraday.close t.faraday;
  t

let empty = create_empty ()

let ready_to_write t =
  let callback = t.when_ready_to_write in
  t.when_ready_to_write <- default_ready_to_write;
  callback ()

let write_char t c =
  Faraday.write_char t.faraday c;
  ready_to_write t

let write_string t ?off ?len s =
  Faraday.write_string ?off ?len t.faraday s;
  ready_to_write t

let write_bigstring t ?off ?len b =
  Faraday.write_bigstring ?off ?len t.faraday b;
  ready_to_write t

let schedule_bigstring t ?off ?len (b:Bigstringaf.t) =
  Faraday.schedule_bigstring ?off ?len t.faraday b;
  ready_to_write t

let flush t kontinue =
  Faraday.flush t.faraday kontinue;
  ready_to_write t

let is_closed t =
  Faraday.is_closed t.faraday

let close_writer t =
  Faraday.close t.faraday;
  ready_to_write t;
;;

let unsafe_faraday t =
  t.faraday

let rec do_execute_read t on_eof on_read =
  match Faraday.operation t.faraday with
  | `Yield           -> ()
  | `Close           ->
    t.read_scheduled <- false;
    t.on_eof         <- default_on_eof;
    t.on_read        <- default_on_read;
    on_eof ()
  | `Writev []       -> assert false
  | `Writev (iovec::_) ->
    t.read_scheduled <- false;
    t.on_eof         <- default_on_eof;
    t.on_read        <- default_on_read;
    let { Httpaf.IOVec.buffer; off; len } = iovec in
    Faraday.shift t.faraday len;
    on_read buffer ~off ~len;
    execute_read t
and execute_read t =
  if t.read_scheduled then do_execute_read t t.on_eof t.on_read

let schedule_read t ~on_eof ~on_read =
  if t.read_scheduled
  then failwith "Body.schedule_read: reader already scheduled";
  if is_closed t
  then do_execute_read t on_eof on_read
  else begin
    t.read_scheduled <- true;
    t.on_eof         <- on_eof;
    t.on_read        <- on_read
  end

let has_pending_output t =
  (* Force another write poll to make sure that the final chunk is emitted for
   * chunk-encoded bodies. *)
  Faraday.has_pending_output t.faraday
  || (Faraday.is_closed t.faraday && t.write_final_data_frame)

let close_reader t =
  Faraday.close t.faraday;
  execute_read t
;;

let when_ready_to_write t callback =
  (* Due to the flow-control window, this function might be called when another
   * callback is already stored. We don't enforce that the currently stored
   * callback is equal to `default_ready_to_write` because it is OK for that
   * not to happen. *)
  if is_closed t then
    callback ()
  else
    t.when_ready_to_write <- callback

let transfer_to_writer t writer ~max_frame_size ~max_bytes stream_id =
  let faraday = t.faraday in
  match Faraday.operation faraday with
  | `Yield -> 0
  | `Close ->
    if t.write_final_data_frame then begin
      t.write_final_data_frame <- false;
      (* Note: we don't need to check if we're flow-controlled here.

         From RFC7540§6.9.1:
           Frames with zero length with the END_STREAM flag set (that is, an
           empty DATA frame) MAY be sent if there is no available space in
           either flow-control window. *)
      let frame_info =
        Serialize.Writer.make_frame_info
          ~max_frame_size
          ~flags:Flags.(set_end_stream default_flags)
          stream_id
      in
      Serialize.Writer.schedule_data writer frame_info ~len:0 Bigstringaf.empty
    end;
    0
  | `Writev iovecs ->
    let buffered = t.buffered_bytes in
    let iovecs   = Httpaf.IOVec.shiftv  iovecs !buffered in
    let lengthv  = Httpaf.IOVec.lengthv iovecs in
    let writev_len = if max_bytes < lengthv then max_bytes else lengthv in
    buffered := !buffered + writev_len;
    let frame_info =
      Serialize.Writer.make_frame_info ~max_frame_size stream_id
    in
    Serialize.Writer.schedule_iovecs writer frame_info ~len:writev_len iovecs;
    Serialize.Writer.flush writer (fun () ->
      Faraday.shift faraday writev_len;
      buffered := !buffered - writev_len);
    writev_len
