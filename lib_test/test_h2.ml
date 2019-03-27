open Test_common
open H2
open H2__

module Headers_tests = struct
  let test_headers_roundtrip_ordering () =
    let headers_list = [ "a", "1"; "b", "2"; "c", "3" ] in

    Alcotest.(check (list (pair string string))) "to_list / of_list"
      Headers.(to_list (of_list headers_list)) headers_list;

    Alcotest.(check (option string)) "get / of_list"
      Headers.(get (of_list [("k", "v1"); ("k", "v2")]) "k")
      (Some "v2");

    Alcotest.(check (option string)) "get / of_rev_list"
      Headers.(get (of_rev_list [("k", "v1"); ("k", "v2")]) "k")
      (Some "v1");

    let headers = Headers.(add_list empty headers_list) in
    Alcotest.(check (option string)) "add / get"
      Headers.(get (add headers "foo" "bar") "foo")
      (Some "bar");

    let hs = Headers.(add (add empty "foo" "bar") "foo" "other") in
    Alcotest.(check (option string)) "add / get"
      Headers.(get hs "foo")
      (Some "other")

  let suite =
    [ "roundtripping"  , `Quick, test_headers_roundtrip_ordering ]
end

module Server_connection_tests = struct
  open Server_connection

  module Read_operation = struct
    type t = [ `Read | `Yield | `Close | `Error of Error.t]

    let pp_hum fmt t =
      let str =
        match t with
        | `Read -> "Read"
        | `Yield -> "Yield"
        | `Error (Error.ConnectionError (e, msg)) ->
          Format.sprintf "ConnectionError: %ld %S" (Error.serialize e) msg
        | `Error (Error.StreamError (stream_id, e)) ->
          Format.sprintf "StreamError on %ld: %ld" stream_id (Error.serialize e)
        | `Close -> "Close"
      in
      Format.pp_print_string fmt str
    ;;
  end

  module Write_operation = struct
    type t = [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]

    let iovecs_to_string iovecs =
      let len = IOVec.lengthv iovecs in
      let bytes = Bytes.create len in
      let dst_off = ref 0 in
      List.iter (fun { IOVec.buffer; off = src_off; len } ->
        Bigstringaf.unsafe_blit_to_bytes buffer ~src_off bytes ~dst_off:!dst_off ~len;
        dst_off := !dst_off + len)
      iovecs;
      Bytes.unsafe_to_string bytes

    let pp_hum fmt t =
      match t with
      | `Write iovecs ->
        Format.fprintf fmt "Write %S" (iovecs_to_string iovecs |> hex_of_string)
      | `Yield -> Format.pp_print_string fmt "Yield"
      | `Close len -> Format.fprintf fmt "Close %i" len

    let to_write_as_string t =
      match t with
      | `Write iovecs -> Some (iovecs_to_string iovecs)
      | `Close _ | `Yield -> None
  end

  let read_operation = Alcotest.of_pp Read_operation.pp_hum
  let write_operation = Alcotest.of_pp Write_operation.pp_hum

  let default_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) ""
  ;;

  let test_initial_reader_state () =
    let t = create default_request_handler in
    Alcotest.(check read_operation) "A new reader wants input"
      `Read (next_read_operation t)

  let test_reader_is_closed_after_eof () =
    let t = create default_request_handler in
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation) "Shutting down a reader closes it"
      `Close (next_read_operation t);

    let t = create default_request_handler in
    let c = read t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read with no input returns 0" 0 c;
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0; in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation) "Shutting down a reader closes it"
      `Close (next_read_operation t)

  let encode_headers hpack_encoder headers =
    let f = Faraday.create 0x1000 in
    Serialize.Writer.encode_headers hpack_encoder f headers;
    Faraday.serialize_to_bigstring f

  let parse_frame_bigstring wire handler =
    let reader = Reader.frame handler in
    let _read =
      Reader.read_with_more reader wire ~off:0 ~len:(Bigstringaf.length wire) Incomplete
    in
    reader

  let parse_frame wire =
    parse_frame_bigstring (bs_of_string wire)

  let preface =
    let f = Faraday.create 0x1000 in
    Serialize.write_connection_preface f;
    Faraday.serialize_to_string f

  let handle_preface t =
    let preface_len = String.length preface in
    let preface =
      read t (Bigstringaf.of_string preface ~off:0 ~len:preface_len) ~off:0 ~len:preface_len
    in
    Alcotest.(check int) "read preface returns preface length" preface_len preface;
    match next_write_operation t with
    | `Write iovecs ->
      let iovec_len = IOVec.lengthv iovecs in
      Alcotest.(check bool "Write more than 0" true (iovec_len > 0));
      report_write_result t (`Ok iovec_len)
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing the connection preface."

  let error_handler ?request:_ error handle =
    let message =
      match error with
      | `Exn exn -> Printexc.to_string exn
      | (#Status.client_error | #Status.server_error) as error -> Status.to_string error
    in
    let body = handle Headers.empty in
    Body.write_string body message;
    Body.close_writer body

  let test_reading_malformed_frame ?(is_failure=false) wire () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let len = String.length wire in
    let bs = Bigstringaf.of_string ~off:0 ~len wire in
    let c = read_eof t bs ~off:0 ~len in
    if is_failure then begin
      Alcotest.(check int)
        "read_eof with bad input (triggers a parse error) reads the header" 10 c;
      match Reader.next (reader t) with
      | `Error _ ->
        Alcotest.(check pass) "bad input triggers an `Error` in the parser state" () ();
      | _ ->
        Alcotest.fail "expected parser to be in an error state"
    end else begin
      Alcotest.(check int) "read_eof with invalid reads the whole frame" len c;
      Alcotest.check read_operation "Shutting down a reader closes it"
        `Close (Reader.next (reader t));
    end

  (* Well-formed HEADERS + CONTINUATION frames. *)
  let header_and_continuation_frames =
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 1l
        ; flags = Flags.(set_end_stream default_flags)
        ; frame_type = Headers
        }
      ; frame_payload =
          Frame.Headers
            ( None
            , encode_headers hpack_encoder
                Headers.(of_list
                  [ ":method", "GET"
                  ; ":scheme", "https"
                  ; ":path", "/"
                  ])
            )
      }
    in
    let continuation =
      { Frame
      . frame_header =
          { headers.frame_header
          with flags = Flags.(default_flags |> set_end_header)
          ; frame_type = Continuation
          }
      ; frame_payload =
          Frame.Continuation (encode_headers hpack_encoder Headers.(of_list ["baz", "qux"]))
      }
    in
    headers, continuation

  let test_send_frame_after_padded_frame () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let headers, continuation = header_and_continuation_frames in
    let padding = Bigstringaf.of_string ~off:0 ~len:10 "0123456789" in
    let headers_wire = Test_common.serialize_frame ~padding headers in
    let headers_length = Bigstringaf.length headers_wire in
    let continuation_wire = Test_common.serialize_frame continuation in
    let continuation_length = Bigstringaf.length continuation_wire in
    let read_headers = read t ~off:0 ~len:headers_length headers_wire in
    Alcotest.(check int) "Read the entire first frame" headers_length read_headers;
    let read_continuation = read t ~off:0 ~len:continuation_length continuation_wire in
    Alcotest.(check int) "Read the entire second frame" continuation_length read_continuation

  let write_frames conn frames =
    List.iter (fun frame ->
      let frame_wire = Test_common.serialize_frame frame in
      let frame_length = Bigstringaf.length frame_wire in
      let read_frame = read conn ~off:0 ~len:frame_length frame_wire in
      Alcotest.(check int) "Read the entire frame" frame_length read_frame)
    frames

  let check_response conn =
    match next_write_operation conn with
    | `Write iovecs ->
      let frames = ref [] in
      ignore @@ parse_frame (Write_operation.iovecs_to_string iovecs) (function
        | Ok frame -> frames := !frames @ [ frame ]
        | Error _ -> Alcotest.fail "expected response frame to parse");
      let frame = List.hd !frames in
      Alcotest.(check int) "Next write operation is a HEADERS frame"
        (Frame.FrameType.serialize Headers)
        (Frame.(frame.frame_header.frame_type |>FrameType.serialize));
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result conn (`Ok iovec_len);
      Alcotest.check write_operation "Writer yields"
        `Yield (next_write_operation conn);
      Alcotest.check read_operation "Reader wants to read"
        `Read (next_read_operation conn)
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing headers."

  let write_frames_and_check_response conn frames =
    write_frames conn frames;
    check_response conn

  let test_continuation_frame () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let headers, continuation = header_and_continuation_frames in
    write_frames_and_check_response t [headers; continuation];
    let new_headers =
      { headers
      with frame_header =
        { headers.frame_header
        with stream_id = 3l
        ; flags = Flags.(set_end_header default_flags)
        }
      }
    in
    write_frames_and_check_response t [new_headers]

  let test_continuation_frame_another_stream () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let headers, continuation = header_and_continuation_frames in
    let continuation =
      { continuation
        (* continuation header on a different stream *)
      with frame_header = { continuation.frame_header with stream_id = 3l }
      }
    in
    write_frames t [headers; continuation];
    match next_write_operation t with
    | `Write iovecs ->
      parse_frame (Write_operation.iovecs_to_string iovecs) (function
      | Ok frame ->
        Alcotest.(check int) "Next write operation is a GOAWAY frame"
          (Frame.FrameType.serialize GoAway)
          (Frame.(frame.frame_header.frame_type |> FrameType.serialize));
        let iovec_len = IOVec.lengthv iovecs in
        report_write_result t (`Ok iovec_len);
        Alcotest.(check write_operation) "Next operation should be close"
          (`Close 0) (next_write_operation t);
        Alcotest.(check bool) "Connection is shutdown" true (is_shutdown t)
      | Error _ ->
          Alcotest.fail "expected response frame to parse")
      |> ignore;
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing a malformed CONTINUATION frame."

  let test_read_frame_size_error () =
    let max_length = String.length preface in
    let config =
      { Config
      . default
      with read_buffer_size = max_length
      }
    in
    let t = create ~config ~error_handler default_request_handler in
    handle_preface t;
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 1l
        ; flags = Flags.default_flags
        ; frame_type = Headers
        }
      ; frame_payload =
          Frame.Headers
            ( None
            , encode_headers hpack_encoder
                Headers.(of_list
                  [ ":method", "GET"
                  ; ":scheme", "https"
                  ; ":path", "/"
                  ; "some_really_long_header_name", "some_really_long_header_value"
                  ])
            )
      }
    in
    let frame_wire = Test_common.serialize_frame headers in
    let frame_length = Bigstringaf.length frame_wire in
    Alcotest.(check bool) "Frame payload is surely over the max length"
      true (frame_length > max_length);
    let read1 = read t ~off:0 ~len:max_length frame_wire in
    Alcotest.(check int) "only read the frame header" 9 read1;
    let read2 = read t ~off:9 ~len:(max_length - 9) frame_wire in
    Alcotest.(check int) "couldn't read more" 0 read2;
    (* Read buffer advanced, contents are not the same anymore. *)
    let read3 = read_eof t ~off:20 ~len:(max_length - 9) frame_wire in
    Alcotest.(check int) "couldn't read more" 0 read3;
    Alcotest.check read_operation "There was a stream error of type FRAME_SIZE_ERROR"
      (`Error Error.(ConnectionError (FrameSizeError, "frame_payload: not enough input")))
      (Reader.next (reader t))

  let test_read_frame_size_error_priority_frame () =
    let max_length = String.length preface in
    let config =
      { Config
      . default
      with read_buffer_size = max_length
      }
    in
    let t = create ~config ~error_handler default_request_handler in
    handle_preface t;
    let frame_header_wire = "000025020000000001"
      |> string_of_hex
      |> bs_of_string
    in
    let frame_payload_wire =
      "6365727461696E6C7900000000000000000000000000000000000000000000000000000000"
      |> string_of_hex
      |> bs_of_string
    in
    let frame_payload_length = Bigstringaf.length frame_payload_wire in
    Alcotest.(check bool) "Frame payload is surely over the max length"
      true (frame_payload_length > max_length);
    let read1 = read t ~off:0 ~len:9 frame_header_wire in
    Alcotest.(check int) "read only the frame header" 9 read1;
    Alcotest.check read_operation "There was a stream error of type FRAME_SIZE_ERROR"
      (`Error Error.(StreamError (1l, FrameSizeError)))
      (Reader.next (reader t));
    (* payload length declared in the frame header *)
    let bytes_to_skip = ref 0x25 in
    let read2 = read t ~off:0 ~len:max_length frame_payload_wire in
    Alcotest.(check int) "keeps advancing over bad input (payload length)" max_length read2;
    bytes_to_skip := !bytes_to_skip - read2;
    let read3 = read t ~off:read2 ~len:!bytes_to_skip frame_payload_wire in
    Alcotest.(check int) "Read last bytes of payload" !bytes_to_skip read3;
    bytes_to_skip := !bytes_to_skip - read3;
    Alcotest.(check int) "No more to read" 0 !bytes_to_skip;
    Alcotest.check read_operation "Stream Error is not reported again, reader wants to read now"
      `Read
      (Reader.next (reader t))

  let test_preface_read_with_more_frames () =
    let t = create ~error_handler default_request_handler in
    let headers, _ = header_and_continuation_frames in
    let frame_wire = Test_common.serialize_frame headers in
    let frame_length = Bigstringaf.length frame_wire in
    let preface_length = String.length preface in
    let preface_and_headers = Bigstringaf.create (frame_length + preface_length)
    in
    let preface_headers_length = Bigstringaf.length preface_and_headers in
    Bigstringaf.blit_from_string
      preface ~src_off:0 preface_and_headers ~dst_off:0 ~len:preface_length;
    Bigstringaf.blit
      frame_wire ~src_off:0 preface_and_headers ~dst_off:preface_length ~len:frame_length;
    let read_preface =
      read t preface_and_headers ~off:0 ~len:preface_headers_length
    in
    Alcotest.(check int) "read preface returns preface length"
      preface_length read_preface;
    let read_more =
      read t preface_and_headers ~off:read_preface ~len:(preface_headers_length - read_preface)
    in
    Alcotest.(check int) "should be able to read the next frame"
      frame_length read_more;
    Alcotest.check read_operation "Reader wants to read" `Read (next_read_operation t)
  let test_settings_frame_unsigned () =
    let settings =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 0l
        ; flags = Flags.default_flags
        ; frame_type = Settings
        }
      ; frame_payload =
        Frame.Settings Settings.[ InitialWindowSize, 1 lsl 31 ]
      }
    in
    let frame_wire = Test_common.serialize_frame settings in
    ignore @@ parse_frame_bigstring frame_wire (function
      | Ok { Frame.frame_payload = Settings [ Settings.InitialWindowSize, v ]; _ } ->
        (* The protocol says it should read a uint32 here, but because the
         * largest value it accepts is 2^31 - 1 we work around that by checking
         * for negative numbers (that have overflown). We avoid adding a new
         * dependency this way, but if we ever want to support it at least we
         * have this test. *)
        Alcotest.(check int) "Window Size value roundtrips in a signed fashion"
          (- 1 lsl 31) v
      | _ -> Alcotest.fail "Expected frame to parse successfully.")

  let test_open_existing_stream () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let priority =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 1l
        ; flags = Flags.default_flags
        ; frame_type = Priority
        }
      ; frame_payload =
          Frame.Priority Priority.default_priority
      }
    in
    write_frames t [priority];
    let open Streams in
    let Connection root = t.streams in
    let root_children = root.children |> PriorityQueue.to_list in
    Alcotest.(check (list int32)) "Stream has been added to the priority tree"
      [1l] (List.map fst root_children);
    let Stream { reqd = old_reqd; _ } = root_children |> List.hd |> snd in
    let headers, _ = header_and_continuation_frames in
    let headers =
      { headers
      with Frame.frame_header =
        { headers.frame_header
        with flags = Flags.(default_flags |> set_end_header |> set_end_stream)
        }
      }
    in
    write_frames t [headers];
    let open Streams in
    let new_root_children = root.children |> PriorityQueue.to_list in
    let Stream { reqd; _ } = new_root_children |> List.hd |> snd in
    Alcotest.(check (list int32)) "Priority tree still only contains one stream"
      [1l] (new_root_children |> List.map fst);
    Alcotest.(check bool) "Reqd is the same" true (old_reqd == reqd)

  let data_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) "Some data"

  let test_dependent_stream () =
    let t = create ~error_handler data_request_handler in
    handle_preface t;
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 1l
        ; flags = Flags.(default_flags |> set_end_header |> set_end_stream)
        ; frame_type = Headers
        }
      ; frame_payload =
          Frame.Headers
            ( None
            , encode_headers hpack_encoder
                Headers.(of_list
                  [ ":method", "GET"
                  ; ":scheme", "https"
                  ; ":path", "/"
                  ])
            )
      }
    in
    let second_headers =
      { Frame
      . frame_header = { headers.frame_header with stream_id = 3l }
      ; frame_payload =
          Frame.Headers
            ( Some { Priority.default_priority with stream_dependency = 1l }
            , encode_headers hpack_encoder
                Headers.(of_list
                  [ ":method", "GET"
                  ; ":scheme", "https"
                  ; ":path", "/"
                  ])
            )
      }
    in
    write_frames t [headers; second_headers];
    let open Streams in
    let Stream first_stream = Streams.get_node t.streams 1l |> opt_exn in
    let first_stream_children = first_stream.children |> PriorityQueue.to_list in
    Alcotest.(check (list int32)) "Stream 3 has been added to the priority tree"
      [3l] (List.map fst first_stream_children);
    match next_write_operation t with
    | `Write iovecs ->
      let frames = ref [] in
      ignore @@ parse_frame (Write_operation.iovecs_to_string iovecs) (function
        | Ok frame -> frames := !frames @ [ frame ]
        | Error _ -> Alcotest.fail "expected response frame to parse");
      Alcotest.(check (list int))
        "HEADERS frames are flushed immediately, along with DATA for stream 1"
        Frame.FrameType.(List.map serialize [Headers; Headers; Data])
        (!frames
        |> List.map (fun { Frame.frame_header; _ } ->
          Frame.(frame_header.frame_type |> FrameType.serialize)));
      let data_frame = List.nth !frames 2 in
      Alcotest.(check int32) "The emitted DATA frame belongs to stream 1"
        1l (data_frame.frame_header.stream_id);
      Server_connection.report_write_result t (`Ok (IOVec.lengthv iovecs));
      frames := [];
      begin match next_write_operation t with
      | `Write iovecs ->
        ignore @@ parse_frame (Write_operation.iovecs_to_string iovecs) (function
          | Ok frame -> frames := !frames @ [ frame ]
          | Error _ -> Alcotest.fail "expected response frame to parse");
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing headers."
      end;
      Alcotest.(check (list int))
        "DATA for stream 3 is flushed next"
        Frame.FrameType.[serialize Data]
        (!frames
        |> List.map (fun { Frame.frame_header; _ } ->
          Frame.(frame_header.frame_type |> FrameType.serialize)));
      let data_frame = List.hd !frames in
      Alcotest.(check int32) "The emitted DATA frame belongs to stream 3"
        3l (data_frame.frame_header.stream_id);
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing headers."

  let server_push_request_handler reqd =
    let request = Request.create `GET "/main.css" in
    let pushed_reqd = Reqd.push reqd request in
    let response = Response.create `OK in
    (* Send the response for / *)
    Reqd.respond_with_string reqd response "Hello";
    (* Send the response for /main.css *)
    Reqd.respond_with_string pushed_reqd response "Hello"

  let test_server_push () =
    let t = create ~error_handler server_push_request_handler in
    handle_preface t;
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame
      . frame_header =
        { payload_length = 0
        ; stream_id = 1l
        ; flags = Flags.(default_flags |> set_end_header |> set_end_stream)
        ; frame_type = Headers
        }
      ; frame_payload =
          Frame.Headers
            ( None
            , encode_headers hpack_encoder
                Headers.(of_list
                  [ ":method", "GET"
                  ; ":scheme", "https"
                  ; ":path", "/"
                  ])
            )
      }
    in
    (* This calls the request handler. We then expect to receive at least 3
     * frames: 1 HEADERS frame for the response, a PUSH_PROMISE frame for the
     * pushed request and its respective HEADERS frame. *)
    write_frames t [headers];
    match next_write_operation t with
    | `Write iovecs ->
      let frames = ref [] in
      ignore @@ parse_frame (Write_operation.iovecs_to_string iovecs) (function
        | Ok frame -> frames := !frames @ [ frame ]
        | Error _ -> Alcotest.fail "expected response frame to parse");
      Alcotest.(check (list int))
        "Next write operation surfaces writes for the PUSH_PROMISE frame and HEADERS / DATA"
        List.(map (fun { Frame.frame_header; _ } ->
          Frame.FrameType.serialize frame_header.frame_type) !frames)
        List.(map Frame.FrameType.serialize Frame.FrameType.[PushPromise; Headers; Headers; Data]);
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result t (`Ok iovec_len);
      begin match next_write_operation t with
      | `Write iovecs ->
        frames := [];
        ignore @@ parse_frame (Write_operation.iovecs_to_string iovecs) (function
          | Ok frame -> frames := !frames @ [ frame ]
          | Error _ -> Alcotest.fail "expected response frame to parse");
        let frame = List.hd !frames in
        Alcotest.(check int)
          "Next write operation surfaces the last DATA frame"
          Frame.FrameType.(serialize frame.frame_header.frame_type)
          Frame.FrameType.(serialize Data);
        Alcotest.(check int32)
          "The last DATA frame is for the server-pushed stream"
          2l
          frame.frame_header.stream_id;
        let iovec_len = IOVec.lengthv iovecs in
        report_write_result t (`Ok iovec_len);
        Alcotest.check write_operation "Writer yields"
          `Yield (next_write_operation t);
        Alcotest.check read_operation "Reader wants to read"
          `Read (next_read_operation t)
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing headers."
      end
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing headers."

  (* data frame with invalid amount of padding *)
  let w = string_of_hex "00000400080000000104AAAAAA"
  (* data frame with frame size error *)
  let w2 = string_of_hex "0080000008000000020648656C6C6F2C20776F726C6421686F77647921"
  (* priority frame with (stream error of) frame size error - header declares 4
   * bytes of payload, not yet totally available on the buffer. *)
  let w3 = string_of_hex "0000040200000000018000"

  (* Testing for https://github.com/inhabitedtype/angstrom/pull/166 *)
  let test_reading_just_header wire () =
    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let wire = bs_of_string wire in
    let wire_length = Bigstringaf.length wire in
    let c = read t wire ~off:0 ~len:wire_length in
    Alcotest.(check int) "read with invalid payload reads only the header" 9 c;
    Alcotest.check read_operation "Reader wants more input to advance and report the stream error"
      `Read
      (Reader.next (reader t));

    let t = create ~error_handler default_request_handler in
    handle_preface t;
    let c = read_eof t wire ~off:0 ~len:wire_length in
    (* Difference between this test and the one above is `read_eof` vs `read` *)
    Alcotest.(check int) "read_eof with invalid payload reads only the header" 9 c;
    Alcotest.check read_operation "Shutting down a reader closes it"
      (`Error Error.(StreamError (1l, FrameSizeError)))
      (Reader.next (reader t))

  (* TODO: test for trailer headers. *)
  (* TODO: test graceful shutdown, allowing lower numbered streas to complete. *)
  let suite =
    [ "initial reader state"  , `Quick, test_initial_reader_state
    ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
    ; "malformed frame", `Quick, test_reading_malformed_frame w
    ; "malformed frame", `Quick, test_reading_malformed_frame ~is_failure:true w2
    ; "malformed frame", `Quick, test_reading_just_header w3
    ; "send frames after a padded frame", `Quick, test_send_frame_after_padded_frame
    ; "continuation frame on the same stream (correct)", `Quick, test_continuation_frame
    ; "continuation frame on another stream", `Quick, test_continuation_frame_another_stream
    ; "frame size error (frame size exceeds `Config.read_buffer_size`)", `Quick, test_read_frame_size_error
    ; "frame size error on a priority frame", `Quick, test_read_frame_size_error_priority_frame
    ; "connection preface read with more frames", `Quick, test_preface_read_with_more_frames
    ; "settings that exceeds the maximum allowed", `Quick, test_settings_frame_unsigned
    ; "open existing stream", `Quick, test_open_existing_stream
    ; "dependent stream", `Quick, test_dependent_stream
    ; "server push", `Quick, test_server_push
    ]
end

let () =
  Alcotest.run "ocaml-h2 unit tests"
    [ "headers", Headers_tests.suite
    ; "server_connection", Server_connection_tests.suite
    ]
