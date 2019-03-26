open Http2af__

let read_all path =
  let file = open_in path in
  try
      really_input_string file (in_channel_length file)
  with exn ->
    close_in file;
    raise exn

let bs_to_string bs =
  let off = 0 in
  let len = Bigstringaf.length bs in
  Bigstringaf.substring ~off ~len bs

let bs_of_string s =
  Bigstringaf.of_string ~off:0 ~len:(String.length s) s

let string_of_hex s = Hex.to_string (`Hex s)

let hex_of_string s =
  let (`Hex hex) = Hex.of_string s in
  String.uppercase_ascii hex

let make_iovecs bs =
  [{ Httpaf.IOVec.buffer = bs; off = 0; len = Bigstringaf.length bs }]

let write_frame ?padding t { Frame.frame_header; frame_payload } =
  let open Serialize in
  let { Frame.flags; stream_id; _ } = frame_header in
  let info = Writer.make_frame_info ~flags ?padding stream_id in
  match frame_payload with
  | Data body ->
    Writer.schedule_data t info body
  | Headers (priority, headers_block) ->
    (* Block already HPACK-encoded. *)
    write_headers_frame t.encoder info ?priority (make_iovecs headers_block)
  | Priority p ->
    Writer.write_priority t info p
  | RSTStream e ->
    Writer.write_rst_stream t info e
  | Settings settings ->
    Writer.write_settings t info settings
  | PushPromise (promised_id, header_block) ->
    write_push_promise_frame t.encoder info ~promised_id (make_iovecs header_block)
  | Ping payload ->
    Writer.write_ping t info payload
  | GoAway (last_stream_id, error, debug_data) ->
    Writer.write_go_away t info ~debug_data ~last_stream_id error
  | WindowUpdate window_size ->
    Writer.write_window_update t info window_size
  | Continuation header_block ->
    write_continuation_frame t.encoder info (make_iovecs header_block)
  | Unknown _ ->
    assert false

let serialize_frame ?padding frame =
  let open Serialize in
  let { Frame.payload_length; _ } = frame.Frame.frame_header in
  let writer = Writer.create payload_length in
  write_frame ?padding writer frame;
  Faraday.serialize_to_bigstring (Writer.faraday writer)

let serialize_frame_string ?padding frame =
  let bs = serialize_frame ?padding frame in
  bs_to_string bs

let opt_exn = function
  | Some x -> x
  | None -> failwith "opt_exn: None"

