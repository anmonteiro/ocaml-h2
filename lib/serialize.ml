open Faraday

type frame_info =
  { flags     : Flags.t
  ; stream_id : Stream_identifier.t
  ; padding   : Bigstringaf.t option
  ; max_frame_payload : int
  }

let write_uint24 t n =
  let write_octet t o =
    write_uint8 t (o land 0xff)
  in
  write_octet t (n lsr 16);
  write_octet t (n lsr 8);
  write_octet t n

let write_frame_header t frame_header =
  let { Frame.payload_length; flags; stream_id; frame_type } = frame_header in
  write_uint24 t payload_length;
  write_uint8 t (Frame.FrameType.serialize frame_type);
  write_uint8 t flags;
  BE.write_uint32 t stream_id

let write_frame_with_padding t info frame_type length writer =
  let header, writer = match info.padding with
  | None ->
    let header =
      { Frame
      . payload_length = length
      ; flags = info.flags
      ; stream_id = info.stream_id
      ; frame_type
      }
    in
    header, writer
  | Some padding ->
    let pad_length = Bigstringaf.length padding in
    let writer' t =
      write_uint8 t pad_length;
      writer t;
      schedule_bigstring ~off:0 ~len:pad_length t padding
    in
    let header =
      { Frame
      . payload_length = length + pad_length + 1
      ; flags = Flags.set_padded info.flags
      ; stream_id = info.stream_id
      ; frame_type
      }
    in
    header, writer'
  in
  write_frame_header t header;
  writer t

let write_data_frame t ?off ?len info body =
  let writer t = write_string t ?off ?len body in
  let length = match len with
  | Some len -> len
  | None -> String.length body
  in
  write_frame_with_padding t info Data length writer

let schedule_data_frame t info ?off ?len bstr =
  let writer t = schedule_bigstring t ?off ?len bstr in
  let length = match len with
  | Some len -> len
  | None -> Bigstringaf.length bstr
  in
  write_frame_with_padding t info Data length writer

let write_priority t {Priority.exclusive; stream_dependency; weight} =
  let stream_dependency_id =
    if exclusive then
      Priority.set_exclusive stream_dependency
    else
      stream_dependency
  in
  BE.write_uint32 t stream_dependency_id;
  (* From RFC7540§6.3:
       An unsigned 8-bit integer representing a priority weight for the stream
       (see Section 5.3). Add one to the value to obtain a weight between 1 and
       256.

    Note: we store priority with values from 1 to 256, so decrement here. *)
  write_uint8 t (weight - 1)

let bounded_schedule_iovecs t ~len iovecs =
  let rec loop t remaining iovecs =
    match remaining, iovecs with
    | 0, _
    | _, [] -> ()
    | remaining, { Httpaf.IOVec.buffer; off; len }::xs ->
      if remaining < len then
        schedule_bigstring t ~off ~len:remaining buffer
      else begin
        schedule_bigstring t ~off ~len buffer;
        loop t (remaining - len) xs
      end
  in
  loop t len iovecs

let write_headers_frame t info ?priority ?len iovecs =
  let len = match len with
  | Some len -> len
  | None -> Httpaf.IOVec.lengthv iovecs
  in
  match priority with
  | None ->
    (* See RFC7540§6.3:
         Just the Header Block Fragment length if no priority. *)
    let writer t =
      bounded_schedule_iovecs t ~len iovecs
    in
    write_frame_with_padding t info Headers len writer
  | Some priority ->
    (* See RFC7540§6.2:
         Exclusive Bit & Stream Dependency (4 octets) + Weight (1 octet) +
         Header Block Fragment length. *)
    let payload_length = len + 5 in
    let info' = { info with flags = Flags.set_priority info.flags } in
    let writer t =
      write_priority t priority;
      bounded_schedule_iovecs t ~len iovecs
    in
    write_frame_with_padding t info' Headers payload_length writer

let write_priority_frame t info priority =
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
      (* See RFC7540§6.3:
           Stream Dependency (4 octets) + Weight (1 octet). *)
    ; payload_length = 5
    ; frame_type =  Priority
    }
  in
  write_frame_header t header;
  write_priority t priority

let write_rst_stream_frame t info e =
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
      (* From RFC7540§6.4:
           The RST_STREAM frame contains a single unsigned, 32-bit integer
           identifying the error code (Section 7). *)
    ; payload_length = 4
    ; frame_type = RSTStream
    }
  in
  write_frame_header t header;
  BE.write_uint32 t (Error.serialize e)

let write_settings_frame t info settings =
  let rec write_settings_payload = function
    | [] -> ()
    | (key, value) :: xs ->
        BE.write_uint16 t (Settings.from_key key);
        BE.write_uint32 t (Int32.of_int value);
        write_settings_payload xs
  in
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
      (* From RFC7540§6.5.1:
           The payload of a SETTINGS frame consists of zero or more parameters,
           each consisting of an unsigned 16-bit setting identifier and an
           unsigned 32-bit value. *)
    ; payload_length = List.length settings * 6
    ; frame_type =  Settings
    }
  in
  write_frame_header t header;
  write_settings_payload settings

let write_push_promise_frame t info ~promised_id ?len iovecs =
  let len = match len with
  | Some len -> len
  | None -> Httpaf.IOVec.lengthv iovecs
  in
  let payload_length =
    (* From RFC7540§6.6:
         The PUSH_PROMISE frame includes the unsigned 31-bit identifier of the
         stream the endpoint plans to create along with a set of headers that
         provide additional context for the stream. *)
    4 + len
  in
  let writer t =
    BE.write_uint32 t promised_id;
    bounded_schedule_iovecs t ~len iovecs
  in
  write_frame_with_padding t info PushPromise payload_length writer

let write_ping_frame t info payload =
  (* From RFC7540§6.7:
       In addition to the frame header, PING frames MUST contain 8 octets of
       opaque data in the payload. *)
  let payload_length = 8 in
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
    ; payload_length
    ; frame_type = Ping
    }
  in
  write_frame_header t header;
  schedule_bigstring ~off:0 ~len:payload_length t payload

let write_go_away_frame t info stream_id error_code debug_data =
  let debug_data_len = Bigstringaf.length debug_data in
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
      (* See RFC7540§6.8:
           Last-Stream-ID (4 octets) + Error Code (4 octets) + Additional Debug
           Data (opaque) *)
    ; payload_length = 8 + debug_data_len
    ; frame_type = GoAway
    }
  in
  write_frame_header t header;
  BE.write_uint32 t stream_id;
  BE.write_uint32 t (Error.serialize error_code);
  schedule_bigstring t ~off:0 ~len:debug_data_len debug_data

let write_window_update_frame t info window_size =
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
      (* From RFC7540§6.9:
           The payload of a WINDOW_UPDATE frame is one reserved bit plus an
           unsigned 31-bit integer indicating the number of octets that the
           sender can transmit in addition to the existing flow-control
           window. *)
    ; payload_length = 4
    ; frame_type = WindowUpdate
    }
  in
  write_frame_header t header;
  BE.write_uint32 t (Int32.of_int window_size)

let write_continuation_frame t info ?len iovecs =
  let len = match len with
  | Some len -> len
  | None -> Httpaf.IOVec.lengthv iovecs
  in
  let header =
    { Frame
    . flags = info.flags
    ; stream_id = info.stream_id
    ; payload_length = len
    ; frame_type = Continuation
    }
  in
  write_frame_header t header;
  bounded_schedule_iovecs t ~len iovecs

(* TODO: write proper settings frame. This function should only write the preface,
 * and `Reader.write_connection_preface` should additionally write the settings.*)
let write_connection_preface t =
  (* From RFC7540§3.5:
       In HTTP/2, each endpoint is required to send a connection preface as a
       final confirmation of the protocol in use and to establish the initial
       settings for the HTTP/2 connection. *)
  write_string t "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n";
  let frame_info =
    { flags = Flags.default_flags
    ; stream_id = Int32.zero
    ; padding = None
      (* Doesn't matter for SETTINGS frames. *)
    ; max_frame_payload = 0
    }
  in
  write_settings_frame t frame_info []

module Writer = struct
  let make_frame_info
    ?padding
    ?(flags=Flags.default_flags)
    ?(max_frame_size=Config.default.read_buffer_size) stream_id =
    { flags
    ; stream_id
    ; padding
    ; max_frame_payload = max_frame_size
    }

  type t =
    { buffer                : Bigstringaf.t
      (* The buffer that the encoder uses for buffered writes. Managed by the
       * control module for the encoder. *)
    ; encoder               : Faraday.t
      (* The encoder that handles encoding for writes. Uses the [buffer]
       * referenced above internally. *)
    ; mutable drained_bytes : int
      (* The number of bytes that were not written due to the output stream
       * being closed before all buffered output could be written. Useful for
       * detecting error cases. *)
    ; headers_block_buffer : Bigstringaf.t
    }

  let create ?(buffer_size=0x800) () =
    let buffer = Bigstringaf.create buffer_size in
    let encoder = Faraday.of_bigstring buffer in
    { buffer
    ; encoder
    ; drained_bytes = 0
      (* TODO: Do we also wanna let this be configurable? *)
    ; headers_block_buffer = Bigstringaf.create 0x1000
    }

  let faraday t = t.encoder

  let write_connection_preface t =
    write_connection_preface t.encoder

  let chunk_data_frames ?(off=0) ~f frame_info total_length =
    let { max_frame_payload; _ } = frame_info in
    if max_frame_payload < total_length then begin
      let rec loop ~off remaining =
        if max_frame_payload < remaining then begin
          (* Note: If we're splitting data into several frames, only the last
           * one should contain the END_STREAM flag, so unset it here if it's
           * set. *)
          let frame_info =
            { frame_info
            with flags = Flags.clear_end_stream frame_info.flags
            }
          in
          f ~off ~len:max_frame_payload frame_info;
          loop ~off:(off + max_frame_payload) (remaining - max_frame_payload)
        end else begin
          f ~off ~len:remaining frame_info
        end
      in
      loop ~off total_length;
    end else begin
      f ~off ~len:total_length frame_info
    end

  let write_data t frame_info ?off ?len str =
    let total_length = match len with
    | Some len -> len
    | None -> String.length str
    in
    chunk_data_frames frame_info ?off total_length
      ~f:(fun ~off ~len frame_info ->
           write_data_frame t.encoder frame_info ~off ~len str)

  let schedule_data t frame_info ?off ?len bstr =
    let total_length = match len with
    | Some len -> len
    | None -> Bigstringaf.length bstr
    in
    chunk_data_frames frame_info ?off total_length ~f:(fun ~off ~len frame_info ->
      schedule_data_frame t.encoder frame_info ~off ~len bstr)

  (* Chunk header block fragments into HEADERS|PUSH_PROMISE + CONTINUATION
   * frames. *)
  let chunk_header_block_fragments t frame_info
    ?(has_priority=false)
    ~(write_frame_fn : Faraday.t ->
      frame_info ->
      ?len:int ->
      Bigstringaf.t iovec list -> unit)
    faraday =
    let block_size = Faraday.pending_bytes faraday in
    let total_length = if has_priority then
      (* See RFC7540§6.2:
           Exclusive Bit & Stream Dependency (4 octets) + Weight (1 octet) +
           Header Block Fragment length. *)
      block_size + 5
    else
      block_size
    in
    let { max_frame_payload; _ } = frame_info in
    if max_frame_payload < total_length then begin
      let headers_block_len = if has_priority then
        max_frame_payload - 5
      else
        max_frame_payload
      in
      let _op = Faraday.serialize faraday (fun iovecs ->
        write_frame_fn t.encoder
          frame_info
          ~len:headers_block_len
          iovecs;
          `Ok headers_block_len)
      in
      let rec loop remaining =
        if max_frame_payload < remaining then begin
          (* Note: Don't reuse flags from frame info as CONTINUATION frames
             only define END_HEADERS.

             From RFC7540§6.10:
               The CONTINUATION frame defines the following flag:

                 END_HEADERS (0x4): When set, bit 2 indicates that this frame
                 ends a header block (Section 4.3). *)
          let frame_info = { frame_info with flags = Flags.default_flags } in
          let _op = Faraday.serialize faraday (fun iovecs ->
            write_continuation_frame t.encoder
              frame_info
              ~len:max_frame_payload
              iovecs;
              `Ok max_frame_payload)
          in
          loop (remaining - max_frame_payload)
        end else begin
          let frame_info =
            { frame_info
            with flags = Flags.(set_end_header default_flags)
            }
          in
          let _op = Faraday.serialize faraday (fun iovecs ->
            write_continuation_frame t.encoder
              frame_info
              ~len:remaining
              iovecs;
            `Ok remaining)
          in
          ()
        end
      in
      loop (block_size - headers_block_len);
    end else begin
      let frame_info =
        { frame_info
        with flags = Flags.set_end_header frame_info.flags
        }
      in
      let _op = Faraday.serialize faraday (fun iovecs ->
        let len = Httpaf.IOVec.lengthv iovecs in
        write_frame_fn t.encoder frame_info ~len iovecs;
        `Ok len)
      in
      ()
    end

  let encode_headers hpack_encoder faraday headers =
    List.iter (fun header ->
      Hpack.Encoder.encode_header hpack_encoder faraday header)
      (Headers.to_hpack_list headers)

  let write_response_headers t hpack_encoder ?priority frame_info response =
    let { Response.status; headers; _ } = response in
    let faraday = Faraday.of_bigstring t.headers_block_buffer in
    (* From RFC7540§8.1.2.4:
         For HTTP/2 responses, a single :status pseudo-header field is defined
         that carries the HTTP status code field (see [RFC7231], Section 6).
         This pseudo-header field MUST be included in all responses; otherwise,
         the response is malformed (Section 8.1.2.6). *)
    Hpack.Encoder.encode_header hpack_encoder faraday
      { Headers
      . name = ":status"
      ; value = Status.to_string (status :> Httpaf.Status.t)
      ; sensitive = false
      };
    encode_headers hpack_encoder faraday headers;
    let has_priority = match priority with
    | Some _ -> true
    | None -> false
    in
    chunk_header_block_fragments
      t
      frame_info
      ~write_frame_fn:(write_headers_frame ?priority)
      ~has_priority
      faraday

  let write_push_promise t hpack_encoder frame_info ~promised_id request =
    let { Request.meth; target; headers } = request in
    let faraday = Faraday.of_bigstring t.headers_block_buffer in
    Hpack.Encoder.encode_header hpack_encoder faraday
      { Headers
      . name = ":method"
      ; value = Httpaf.Method.to_string meth
      ; sensitive = false
      };
    Hpack.Encoder.encode_header hpack_encoder faraday
      { Headers
      . name = ":path"
      ; value = target
      ; sensitive = false
      };
    encode_headers hpack_encoder faraday headers;
    chunk_header_block_fragments
      t
      frame_info
      ~write_frame_fn:(write_push_promise_frame ~promised_id)
      faraday

  let write_rst_stream t frame_info e =
    write_rst_stream_frame t.encoder frame_info e

  let write_window_update t frame_info n =
    write_window_update_frame t.encoder frame_info n

  let schedule_iovecs t ~len frame_info iovecs =
    let writer t ~iovecs =
      bounded_schedule_iovecs t ~len iovecs
    in
    chunk_data_frames frame_info len ~f:(fun ~off ~len frame_info ->
      write_frame_with_padding t.encoder
        frame_info
        Data
        len
        (writer ~iovecs:(Httpaf.IOVec.shiftv iovecs off)))

  let write_priority t frame_info priority =
    write_priority_frame t.encoder frame_info priority

  let write_settings t frame_info settings =
    write_settings_frame t.encoder frame_info settings

  let write_ping t frame_info payload =
    write_ping_frame t.encoder frame_info payload

  let write_go_away t frame_info ~debug_data ~last_stream_id error =
    write_go_away_frame t.encoder frame_info last_stream_id error debug_data

  let flush t f =
    flush t.encoder f

  let yield t =
    Faraday.yield t.encoder

  let close t =
    Faraday.close t.encoder

  let close_and_drain t =
    Faraday.close t.encoder;
    let drained = Faraday.drain t.encoder in
    t.drained_bytes <- t.drained_bytes + drained

  let is_closed t =
    Faraday.is_closed t.encoder

  let drained_bytes t =
    t.drained_bytes

  let report_result t result =
    match result with
    | `Closed -> close t
    | `Ok len -> shift t.encoder len

  let next t =
    match Faraday.operation t.encoder with
    | `Close         -> `Close (drained_bytes t)
    | `Yield         -> `Yield
    | `Writev iovecs -> `Write iovecs
end
