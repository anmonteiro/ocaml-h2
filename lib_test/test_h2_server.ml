open Test_common
open H2
open H2__

module Server_connection_tests = struct
  open Server_connection

  module Read_operation = struct
    type t =
      [ `Read
      | `Close
      | `Error of Error.t
      ]

    let pp_hum fmt t =
      let str =
        match t with
        | `Read ->
          "Read"
        | `Error (Error.ConnectionError (e, msg)) ->
          Format.sprintf "ConnectionError: %ld %S" (Error_code.serialize e) msg
        | `Error (Error.StreamError (stream_id, e)) ->
          Format.sprintf
            "StreamError on %ld: %ld"
            stream_id
            (Error_code.serialize e)
        | `Close ->
          "Close"
      in
      Format.pp_print_string fmt str
  end

  module Write_operation = struct
    type t =
      [ `Write of Bigstringaf.t IOVec.t list
      | `Yield
      | `Close of int
      ]

    let iovecs_to_string iovecs =
      let len = IOVec.lengthv iovecs in
      let bytes = Bytes.create len in
      let dst_off = ref 0 in
      List.iter
        (fun { IOVec.buffer; off = src_off; len } ->
          Bigstringaf.unsafe_blit_to_bytes
            buffer
            ~src_off
            bytes
            ~dst_off:!dst_off
            ~len;
          dst_off := !dst_off + len)
        iovecs;
      Bytes.unsafe_to_string bytes

    let pp_hum fmt t =
      match t with
      | `Write iovecs ->
        Format.fprintf fmt "Write %S" (iovecs_to_string iovecs |> hex_of_string)
      | `Yield ->
        Format.pp_print_string fmt "Yield"
      | `Close len ->
        Format.fprintf fmt "Close %i" len

    let to_write_as_string t =
      match t with
      | `Write iovecs ->
        Some (iovecs_to_string iovecs)
      | `Close _ | `Yield ->
        None
  end

  let read_operation = Alcotest.of_pp Read_operation.pp_hum

  let write_operation = Alcotest.of_pp Write_operation.pp_hum

  let default_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) ""

  let test_initial_reader_state () =
    let t = create default_request_handler in
    Alcotest.(check read_operation)
      "A new reader wants input"
      `Read
      (next_read_operation t)

  let test_reader_is_closed_after_eof () =
    let t = create default_request_handler in
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation)
      "Shutting down a reader closes it"
      `Close
      (next_read_operation t);
    let t = create default_request_handler in
    let c = read t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read with no input returns 0" 0 c;
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation)
      "Shutting down a reader closes it"
      `Close
      (next_read_operation t)

  let preface ?(settings = []) () =
    let writer = Serialize.Writer.create 0x400 in
    Serialize.Writer.write_connection_preface writer settings;
    Faraday.serialize_to_string (Serialize.Writer.faraday writer)

  let empty_preface = preface ()

  let handle_preface ?settings t =
    let preface = preface ?settings () in
    let preface_len = String.length preface in
    let preface = read t (bs_of_string preface) ~off:0 ~len:preface_len in
    Alcotest.(check int)
      "read preface returns preface length"
      preface_len
      preface;
    match next_write_operation t with
    | `Write iovecs ->
      let iovec_len = IOVec.lengthv iovecs in
      Alcotest.(check bool "Write more than 0" true (iovec_len > 0));
      report_write_result t (`Ok iovec_len)
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing the \
         connection preface."

  let create_and_handle_preface ?settings ?error_handler ?config request_handler
    =
    let t = create ?config ?error_handler request_handler in
    handle_preface ?settings t;
    t

  let read_string t str =
    let len = String.length str in
    let input = Bigstringaf.of_string str ~off:0 ~len in
    let c = read t input ~off:0 ~len in
    Alcotest.(check int) "read consumes all input" len c

  let request_to_string ?body request =
    let has_body = match body with None -> false | Some _ -> true in
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let writer = Serialize.Writer.create 0x400 in
    let frame_info =
      Writer.make_frame_info
        ~flags:
          (if has_body then
             Flags.default_flags
          else
            Flags.(default_flags |> set_end_stream))
        1l
    in
    Serialize.Writer.write_request_headers
      writer
      hpack_encoder
      ~priority:Priority.default_priority
      frame_info
      request;
    Faraday.serialize_to_string (Serialize.Writer.faraday writer)

  let read_request ?body t request =
    let request_string = request_to_string ?body request in
    read_string t request_string

  let response_to_string t ?body response =
    let has_body = match body with None -> false | Some _ -> true in
    let writer = Serialize.Writer.create 0x400 in
    let frame_info =
      Writer.make_frame_info
        ~flags:
          (if has_body then
             Flags.default_flags
          else
            Flags.(default_flags |> set_end_stream))
        1l
    in
    Serialize.Writer.write_response_headers
      writer
      t.hpack_encoder
      frame_info
      response;
    (match body with
    | None ->
      ()
    | Some body ->
      Serialize.Writer.write_data
        writer
        { frame_info with flags = Flags.(default_flags |> set_end_stream) }
        body);
    Faraday.serialize_to_string (Serialize.Writer.faraday writer)

  let write_string t ?(msg = "frames written") str =
    let len = String.length str in
    Alcotest.(check (option string))
      msg
      (Some (str |> hex_of_string))
      (next_write_operation t
      |> Write_operation.to_write_as_string
      |> Option.map hex_of_string);
    report_write_result t (`Ok len)

  let write_response t ?body response =
    let response_string = response_to_string t ?body response in
    write_string t ~msg:"Response written" response_string

  let write_eof t = report_write_result t `Closed

  let ready_to_read t =
    Alcotest.check
      read_operation
      "Reader wants to read"
      `Read
      (next_read_operation t)

  let writer_yields t =
    Alcotest.check
      write_operation
      "Writer yields"
      `Yield
      (next_write_operation t)

  let writer_closed ?(unread = 0) t =
    Alcotest.(check write_operation)
      "Next operation should be `Close"
      (`Close unread)
      (next_write_operation t)

  let error_handler ?request:_ error handle =
    let message =
      match error with
      | `Exn exn ->
        Printexc.to_string exn
      | (#Status.client_error | #Status.server_error) as error ->
        Status.to_string error
    in
    let body = handle Headers.empty in
    Body.write_string body message;
    Body.close_writer body

  let test_reading_malformed_frame ?(is_failure = false) wire () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let len = String.length wire in
    let bs = Bigstringaf.of_string ~off:0 ~len wire in
    let c = read_eof t bs ~off:0 ~len in
    if is_failure then (
      Alcotest.(check int)
        "read_eof with bad input (triggers a parse error) reads the header"
        10
        c;
      match Reader.next t.reader with
      | `Error _ ->
        Alcotest.(check pass)
          "bad input triggers an `Error` in the parser state"
          ()
          ()
      | _ ->
        Alcotest.fail "expected parser to be in an error state")
    else (
      Alcotest.(check int) "read_eof with invalid reads the whole frame" len c;
      Alcotest.check
        read_operation
        "Shutting down a reader closes it"
        `Close
        (Reader.next t.reader))

  (* Well-formed HEADERS + CONTINUATION frames. *)
  let header_and_continuation_frames =
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.(set_end_stream default_flags)
          ; frame_type = Headers
          }
      ; frame_payload =
          Frame.Headers
            ( Priority.default_priority
            , encode_headers
                hpack_encoder
                Headers.(
                  of_list [ ":method", "GET"; ":scheme", "https"; ":path", "/" ])
            )
      }
    in
    let continuation =
      { Frame.frame_header =
          { headers.frame_header with
            flags = Flags.(default_flags |> set_end_header)
          ; frame_type = Continuation
          }
      ; frame_payload =
          Frame.Continuation
            (encode_headers hpack_encoder Headers.(of_list [ "baz", "qux" ]))
      }
    in
    headers, continuation

  let test_send_frame_after_padded_frame () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let headers, continuation = header_and_continuation_frames in
    let padding = Bigstringaf.of_string ~off:0 ~len:10 "0123456789" in
    let headers_wire = Test_common.serialize_frame ~padding headers in
    let headers_length = Bigstringaf.length headers_wire in
    let continuation_wire = Test_common.serialize_frame continuation in
    let continuation_length = Bigstringaf.length continuation_wire in
    let read_headers = read t ~off:0 ~len:headers_length headers_wire in
    Alcotest.(check int)
      "Read the entire first frame"
      headers_length
      read_headers;
    let read_continuation =
      read t ~off:0 ~len:continuation_length continuation_wire
    in
    Alcotest.(check int)
      "Read the entire second frame"
      continuation_length
      read_continuation

  let read_frames conn frames =
    List.iter
      (fun frame ->
        let frame_wire = Test_common.serialize_frame frame in
        let frame_length = Bigstringaf.length frame_wire in
        let read_frame = read conn ~off:0 ~len:frame_length frame_wire in
        Alcotest.(check int) "Read the entire frame" frame_length read_frame)
      frames

  let check_response conn =
    match next_write_operation conn with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      let frame = List.hd frames in
      Alcotest.(check int)
        "Next write operation is a HEADERS frame"
        (Frame.FrameType.serialize Headers)
        Frame.(frame.frame_header.frame_type |> FrameType.serialize);
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result conn (`Ok iovec_len);
      writer_yields conn;
      ready_to_read conn
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing \
         headers."

  let test_continuation_frame () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let headers, continuation = header_and_continuation_frames in
    read_frames t [ headers; continuation ];
    write_response t (Response.create `OK);
    let new_headers =
      { headers with
        frame_header =
          { headers.frame_header with
            stream_id = 3l
          ; flags = Flags.(set_end_header default_flags)
          }
      }
    in
    read_frames t [ new_headers ];
    check_response t

  let test_continuation_frame_another_stream () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let headers, continuation = header_and_continuation_frames in
    let continuation =
      { continuation (* continuation header on a different stream *) with
        frame_header = { continuation.frame_header with stream_id = 3l }
      }
    in
    read_frames t [ headers; continuation ];
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      let frame = List.hd frames in
      Alcotest.(check int)
        "Next write operation is a GOAWAY frame"
        (Frame.FrameType.serialize GoAway)
        Frame.(frame.frame_header.frame_type |> FrameType.serialize);
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result t (`Ok iovec_len);
      writer_closed t;
      Alcotest.(check bool) "Connection is shutdown" true (is_closed t)
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing a \
         malformed CONTINUATION frame."

  let test_read_frame_size_error () =
    let max_length = String.length (preface ()) in
    let config = { Config.default with read_buffer_size = max_length } in
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.default_flags
          ; frame_type = Headers
          }
      ; frame_payload =
          Frame.Headers
            ( Priority.default_priority
            , encode_headers
                hpack_encoder
                Headers.(
                  of_list
                    [ ":method", "GET"
                    ; ":scheme", "https"
                    ; ":path", "/"
                    ; ( "some_really_long_header_name"
                      , "some_really_long_header_value" )
                    ]) )
      }
    in
    let frame_wire = Test_common.serialize_frame headers in
    let frame_length = Bigstringaf.length frame_wire in
    Alcotest.(check bool)
      "Frame payload is surely over the max length"
      true
      (frame_length > max_length);
    let t =
      create_and_handle_preface ~config ~error_handler default_request_handler
    in
    let read1 = read t ~off:0 ~len:max_length frame_wire in
    Alcotest.(check int) "only read the frame header" 9 read1;
    let read2 = read t ~off:9 ~len:(max_length - 9) frame_wire in
    Alcotest.(check int) "advances over fed input" (max_length - 9) read2;
    let read3 =
      read t ~off:(read1 + read2) ~len:(frame_length - max_length) frame_wire
    in
    Alcotest.(check int)
      "advances over more input"
      (frame_length - max_length)
      read3;
    Alcotest.check
      read_operation
      "Reader wants to read"
      `Read
      (Reader.next t.reader);
    (* #2 *)
    let t =
      create_and_handle_preface ~config ~error_handler default_request_handler
    in
    let read1 = read t ~off:0 ~len:max_length frame_wire in
    Alcotest.(check int) "only read the frame header" 9 read1;
    let read2 = read t ~off:9 ~len:(max_length - 9) frame_wire in
    Alcotest.(check int) "advances over fed input" (max_length - 9) read2;
    (* Read buffer advanced, contents are not the same anymore. *)
    let read3 =
      read_eof
        t
        ~off:(read1 + read2)
        ~len:(frame_length - max_length - (* random *) 5)
        frame_wire
    in
    Alcotest.(check int)
      "advances over more input"
      (frame_length - max_length - (* random *) 5)
      read3;
    Alcotest.check
      read_operation
      "There was a connection error of type FRAME_SIZE_ERROR"
      (`Error Error.(ConnectionError (FrameSizeError, "")))
      (Reader.next t.reader)

  let test_read_frame_size_error_priority_frame () =
    let max_length = String.length empty_preface in
    let config = { Config.default with read_buffer_size = max_length } in
    let t =
      create_and_handle_preface ~config ~error_handler default_request_handler
    in
    let frame_header_wire =
      "000025020000000001" |> string_of_hex |> bs_of_string
    in
    let frame_payload_wire =
      "6365727461696E6C7900000000000000000000000000000000000000000000000000000000"
      |> string_of_hex
      |> bs_of_string
    in
    let frame_payload_length = Bigstringaf.length frame_payload_wire in
    Alcotest.(check bool)
      "Frame payload is surely over the max length"
      true
      (frame_payload_length > max_length);
    let read1 = read t ~off:0 ~len:9 frame_header_wire in
    Alcotest.(check int) "read only the frame header" 9 read1;
    Alcotest.check
      read_operation
      "There was a stream error of type FRAME_SIZE_ERROR"
      (`Error Error.(StreamError (1l, FrameSizeError)))
      (Reader.next t.reader);
    (* payload length declared in the frame header *)
    let bytes_to_skip = ref 0x25 in
    let read2 = read t ~off:0 ~len:max_length frame_payload_wire in
    Alcotest.(check int)
      "keeps advancing over bad input (payload length)"
      max_length
      read2;
    bytes_to_skip := !bytes_to_skip - read2;
    let read3 = read t ~off:read2 ~len:!bytes_to_skip frame_payload_wire in
    Alcotest.(check int) "Read last bytes of payload" !bytes_to_skip read3;
    bytes_to_skip := !bytes_to_skip - read3;
    Alcotest.(check int) "No more to read" 0 !bytes_to_skip;
    Alcotest.check
      read_operation
      "Stream Error is not reported again, reader wants to read now"
      `Read
      (Reader.next t.reader)

  let test_preface_read_with_more_frames () =
    let t = create ~error_handler default_request_handler in
    let headers, _ = header_and_continuation_frames in
    let frame_wire = Test_common.serialize_frame headers in
    let frame_length = Bigstringaf.length frame_wire in
    let preface_length = String.length empty_preface in
    let preface_and_headers =
      Bigstringaf.create (frame_length + preface_length)
    in
    let preface_headers_length = Bigstringaf.length preface_and_headers in
    Bigstringaf.blit_from_string
      empty_preface
      ~src_off:0
      preface_and_headers
      ~dst_off:0
      ~len:preface_length;
    Bigstringaf.blit
      frame_wire
      ~src_off:0
      preface_and_headers
      ~dst_off:preface_length
      ~len:frame_length;
    let read_preface =
      read t preface_and_headers ~off:0 ~len:preface_headers_length
    in
    Alcotest.(check int)
      "read preface returns preface length"
      (preface_length + frame_length)
      read_preface;
    Alcotest.check
      read_operation
      "Reader wants to read"
      `Read
      (next_read_operation t)

  let test_settings_frame_unsigned () =
    let settings =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 0l
          ; flags = Flags.default_flags
          ; frame_type = Settings
          }
      ; frame_payload =
          Frame.Settings Settings.[ InitialWindowSize (Int32.shift_left 1l 31) ]
      }
    in
    let frame_wire = Test_common.serialize_frame settings in
    match parse_frames_bigstring frame_wire with
    | [ { Frame.frame_payload = Settings [ Settings.InitialWindowSize v ]; _ } ]
      ->
      (* The protocol says it should read a uint32 here, but because the
       * largest value it accepts is 2^31 - 1 we work around that by checking
       * for negative numbers (that have overflown). We avoid adding a new
       * dependency this way, but if we ever want to support it at least we
       * have this test. *)
      Alcotest.(check int32)
        "Window Size value roundtrips in a signed fashion"
        (Int32.shift_left (-1l) 31)
        v
    | _ ->
      Alcotest.fail "Expected frame to parse successfully."

  let test_open_existing_stream () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let priority =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.default_flags
          ; frame_type = Priority
          }
      ; frame_payload = Frame.Priority Priority.default_priority
      }
    in
    read_frames t [ priority ];
    let open Scheduler in
    let (Connection root) = t.streams in
    let root_children = root.children |> PriorityQueue.to_list in
    Alcotest.(check (list int32))
      "Stream has been added to the priority tree"
      [ 1l ]
      (List.map fst root_children);
    let (Stream { descriptor = old_reqd; _ }) =
      root_children |> List.hd |> snd
    in
    let headers, _ = header_and_continuation_frames in
    let headers =
      { headers with
        Frame.frame_header =
          { headers.frame_header with
            flags = Flags.(default_flags |> set_end_header |> set_end_stream)
          }
      }
    in
    read_frames t [ headers ];
    let open Scheduler in
    let new_root_children = root.children |> PriorityQueue.to_list in
    let (Stream { descriptor; _ }) = new_root_children |> List.hd |> snd in
    Alcotest.(check (list int32))
      "Priority tree still only contains one stream"
      [ 1l ]
      (new_root_children |> List.map fst);
    Alcotest.(check bool) "Reqd is the same" true (old_reqd == descriptor)

  let data_request_handler reqd =
    Reqd.respond_with_string reqd (Response.create `OK) "Some data"

  let test_dependent_stream () =
    let t = create_and_handle_preface ~error_handler data_request_handler in
    let hpack_encoder = Hpack.Encoder.create 4096 in
    let headers =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.(default_flags |> set_end_header |> set_end_stream)
          ; frame_type = Headers
          }
      ; frame_payload =
          Frame.Headers
            ( Priority.default_priority
            , encode_headers
                hpack_encoder
                Headers.(
                  of_list [ ":method", "GET"; ":scheme", "https"; ":path", "/" ])
            )
      }
    in
    let second_headers =
      { Frame.frame_header = { headers.frame_header with stream_id = 3l }
      ; frame_payload =
          Frame.Headers
            ( { Priority.default_priority with stream_dependency = 1l }
            , encode_headers
                hpack_encoder
                Headers.(
                  of_list [ ":method", "GET"; ":scheme", "https"; ":path", "/" ])
            )
      }
    in
    read_frames t [ headers; second_headers ];
    let open Scheduler in
    let (Stream first_stream) = Scheduler.get_node t.streams 1l |> opt_exn in
    let first_stream_children =
      first_stream.children |> PriorityQueue.to_list
    in
    Alcotest.(check (list int32))
      "Stream 3 has been added to the priority tree"
      [ 3l ]
      (List.map fst first_stream_children);
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      Alcotest.(check (list int))
        "HEADERS frames are flushed immediately, along with DATA for stream 1"
        Frame.FrameType.(List.map serialize [ Headers; Headers; Data ])
        (frames
        |> List.map (fun { Frame.frame_header; _ } ->
               Frame.(frame_header.frame_type |> FrameType.serialize)));
      let data_frame = List.nth frames 2 in
      Alcotest.(check int32)
        "The emitted DATA frame belongs to stream 1"
        1l
        data_frame.frame_header.stream_id;
      Server_connection.report_write_result t (`Ok (IOVec.lengthv iovecs));
      (match next_write_operation t with
      | `Write iovecs ->
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        Alcotest.(check (list int))
          "DATA for stream 3 is flushed next"
          Frame.FrameType.[ serialize Data ]
          (frames
          |> List.map (fun { Frame.frame_header; _ } ->
                 Frame.(frame_header.frame_type |> FrameType.serialize)));
        let data_frame = List.hd frames in
        Alcotest.(check int32)
          "The emitted DATA frame belongs to stream 3"
          3l
          data_frame.frame_header.stream_id
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing \
           headers.")
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing \
         headers."

  let server_push_request_handler reqd =
    let request = Request.create `GET ~scheme:"http" "/main.css" in
    let pushed_reqd =
      match Reqd.push reqd request with
      | Ok reqd ->
        reqd
      | Error _ ->
        Alcotest.fail "Expected `push` to succeed"
    in
    let response = Response.create `OK in
    (* Send the response for / *)
    Reqd.respond_with_string reqd response "Hello";
    (* Send the response for /main.css *)
    Reqd.respond_with_string pushed_reqd response "Hello"

  let test_server_push () =
    let t =
      create_and_handle_preface ~error_handler server_push_request_handler
    in
    let request = Request.create ~scheme:"https" `GET "/" in
    (* This calls the request handler. We then expect to receive at least 3
     * frames: 1 HEADERS frame for the response, a PUSH_PROMISE frame for the
     * pushed request and its respective HEADERS frame. *)
    read_request t request;
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      Alcotest.(check (list int))
        "Next write operation surfaces writes for the PUSH_PROMISE frame and \
         HEADERS / DATA"
        List.(
          map
            (fun { Frame.frame_header; _ } ->
              Frame.FrameType.serialize frame_header.frame_type)
            frames)
        List.(
          map
            Frame.FrameType.serialize
            Frame.FrameType.[ PushPromise; Headers; Headers; Data ]);
      let (Stream pushed_stream) = opt_exn (Scheduler.get_node t.streams 2l) in
      Alcotest.(check int32)
        "Pushed stream has a stream dependency on the parent stream"
        1l
        pushed_stream.priority.stream_dependency;
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result t (`Ok iovec_len);
      (match next_write_operation t with
      | `Write iovecs ->
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
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
        writer_yields t;
        ready_to_read t
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing \
           headers.")
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing \
         headers."

  (* data frame with invalid amount of padding *)
  let w = string_of_hex "00000400080000000104AAAAAA"

  (* data frame with frame size error *)
  let w2 =
    string_of_hex "0080000008000000020648656C6C6F2C20776F726C6421686F77647921"

  (* priority frame with (stream error of) frame size error - header declares 4
   * bytes of payload, not yet totally available on the buffer. *)
  let w3 = string_of_hex "0000040200000000018000"

  (* Testing for https://github.com/inhabitedtype/angstrom/pull/166 *)
  let test_reading_just_header wire () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let wire = bs_of_string wire in
    let wire_length = Bigstringaf.length wire in
    let c = read t wire ~off:0 ~len:wire_length in
    Alcotest.(check int) "read with invalid payload reads only the header" 9 c;
    Alcotest.check
      read_operation
      "Reader wants more input to advance and report the stream error"
      `Read
      (Reader.next t.reader);
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let c = read_eof t wire ~off:0 ~len:wire_length in
    (* Difference between this test and the one above is `read_eof` vs `read` *)
    Alcotest.(check int)
      "read_eof with invalid payload reads only the header"
      9
      c;
    Alcotest.check
      read_operation
      "Shutting down a reader closes it"
      (`Error Error.(StreamError (1l, FrameSizeError)))
      (Reader.next t.reader)

  let test_connect () =
    let error_handler_called = ref false in
    let error_handler ?request:_ error handle =
      error_handler_called := true;
      Alcotest.(check bool) "request was malformed" true (error = `Bad_request);
      let body = handle Headers.empty in
      Body.write_string body "";
      Body.close_writer body
    in
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let request =
      Request.create
        ~headers:Headers.(of_list [ ":authority", "foo.com:8080" ])
        ~scheme:"https"
        `CONNECT
        "/"
    in
    read_request t request;
    write_response t (Response.create `OK);
    writer_yields t;
    Alcotest.(check bool)
      "error handler was not called"
      false
      !error_handler_called

  let test_connect_malformed () =
    let error_handler_called = ref false in
    let error_handler ?request:_ error handle =
      error_handler_called := true;
      Alcotest.(check bool) "request was malformed" true (error = `Bad_request);
      let body = handle Headers.empty in
      Body.write_string body "";
      Body.close_writer body
    in
    let t = create_and_handle_preface ~error_handler default_request_handler in
    (* CONNECT is malformed if it doesn't include the `:authority`
     * pseudo-header. Additionally, the `:scheme` and `:path` pseudo-headers
     * must be omitted, but we take care of that when serializing. See
     * RFC7540§8.3. *)
    let request = Request.create ~scheme:"https" `CONNECT "/" in
    read_request t request;
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      let frame = List.hd frames in
      Alcotest.(check int)
        "Next write operation is a HEADERS frame"
        (Frame.FrameType.serialize Headers)
        Frame.(frame.frame_header.frame_type |> FrameType.serialize);
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result t (`Ok iovec_len);
      Alcotest.(check bool)
        "error handler was called"
        true
        !error_handler_called
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing \
         headers."

  let test_client_max_concurrent_streams () =
    (* From RFC7540§5.1.2: [...] clients specify the maximum number of
     * concurrent streams the server can initiate, and servers specify the
     * maximum number of concurrent streams the client can initiate.
     *
     * Note: in this test, the client is saying the server is not allowed to
     * initiate streams. The client, however, is. *)
    let t =
      create_and_handle_preface
        ~settings:[ MaxConcurrentStreams 0l ]
        ~error_handler
        default_request_handler
    in
    let request = Request.create ~scheme:"https" `GET "/" in
    read_request t request;
    write_response t (Response.create `OK)

  let streaming_handler ?(flush = false) response writes reqd =
    let request_body = Reqd.request_body reqd in
    Body.close_reader request_body;
    let body =
      Reqd.respond_with_streaming ~flush_headers_immediately:flush reqd response
    in
    let rec write writes =
      match writes with
      | [] ->
        Body.close_writer body
      | w :: ws ->
        Body.write_string body w;
        Body.flush body (fun () -> write ws)
    in
    write writes

  let test_empty_fixed_streaming_response () =
    let request = Request.create ~scheme:"http" `GET "/" in
    let response =
      Response.create `OK ~headers:(Headers.of_list [ "content-length", "0" ])
    in
    let t =
      create_and_handle_preface ~error_handler (streaming_handler response [])
    in
    read_request t request;
    write_response t ~body:"" response;
    writer_yields t

  let test_h2c () =
    let settings_payload = Settings.[ EnablePush 0; MaxConcurrentStreams 2l ] in
    let f = Faraday.create 100 in
    Settings.write_settings_payload f settings_payload;
    let serialized_settings = Faraday.serialize_to_string f in
    let http_request =
      Httpaf.Request.create
        ~headers:
          (Httpaf.Headers.of_list
             [ "Connection", "Upgrade, HTTP2-Settings"
             ; "Upgrade", "h2c"
             ; ( "HTTP2-Settings"
               , Base64.(
                   encode_string ~alphabet:uri_safe_alphabet serialized_settings)
               )
             ; "Host", "localhost"
             ])
        `GET
        "/"
    in
    let request_handler_called = ref false in
    match
      create_h2c ~http_request (fun _ -> request_handler_called := true)
    with
    | Ok t ->
      Alcotest.(check bool)
        "Request handler called"
        true
        !request_handler_called;
      Alcotest.(check bool)
        "Connection settings were set as per the incoming settings"
        true
        (t.settings
        = { Settings.default with
            enable_push = false
          ; max_concurrent_streams = 2l
          });
      (match next_write_operation t with
      | `Write iovecs ->
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
        Alcotest.(check int)
          "Next write operation is a SETTINGS frame (server connection preface)"
          (Frame.FrameType.serialize Settings)
          Frame.(frame.frame_header.frame_type |> FrameType.serialize)
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing \
           headers.")
    | Error msg ->
      Alcotest.fail msg

  let test_nonzero_content_length_no_data_frames () =
    let request =
      Request.create
        ~headers:(Headers.of_list [ "content-length", "1234" ])
        ~scheme:"http"
        `GET
        "/"
    in
    let t = create_and_handle_preface ~error_handler default_request_handler in
    read_request t request;
    write_response t ?body:None (Response.create `OK);
    writer_yields t

  let test_unexpected_eof () =
    let t = create_and_handle_preface ~error_handler default_request_handler in
    let request = Request.create ~scheme:"http" `GET "/" in
    read_request t request;
    write_eof t;
    writer_closed t ~unread:10

  let test_read_frame_size_error_unknown_frame () =
    (* Enough for a frame header *)
    let max_length = String.length (preface ()) in
    let config = { Config.default with read_buffer_size = max_length } in
    let t =
      create_and_handle_preface ~config ~error_handler default_request_handler
    in
    let frame =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.default_flags
          ; frame_type = Unknown 80
          }
      ; frame_payload =
          Frame.Unknown
            (80, Bigstringaf.of_string ~off:0 ~len:40 (String.make 40 'a'))
      }
    in
    let frame_wire = Test_common.serialize_frame frame in
    let frame_length = Bigstringaf.length frame_wire in
    Alcotest.(check bool)
      "Frame payload is surely over the max length"
      true
      (frame_length > max_length);
    let read1 = read t ~off:0 ~len:max_length frame_wire in
    Alcotest.(check int) "only read the frame header" 9 read1;
    let read2 = read t ~off:9 ~len:(max_length - 9) frame_wire in
    Alcotest.(check int) "advances over fed input" (max_length - 9) read2;
    let read3 =
      read t ~off:(read1 + read2) ~len:(frame_length - max_length) frame_wire
    in
    Alcotest.(check int)
      "advances over more input"
      (frame_length - max_length)
      read3;
    Alcotest.check
      read_operation
      "Reader wants to read, unknown frame type is ignored"
      `Read
      (Reader.next t.reader)

  let test_reading_request_body () =
    let body_read_called = ref false in
    let body_eof_called = ref false in
    let request = Request.create ~scheme:"http" `GET "/" in
    let response =
      Response.create `OK ~headers:(Headers.of_list [ "content-length", "0" ])
    in
    let request_handler reqd =
      let request_body = Reqd.request_body reqd in
      Body.schedule_read
        request_body
        ~on_eof:ignore
        ~on_read:(fun _bs ~off:_ ~len:_ ->
          body_read_called := true;
          Alcotest.(check bool)
            "Response body isn't closed (yet) when reading"
            false
            (Body.is_closed request_body);
          Body.schedule_read
            ~on_read:(fun _ ~off:_ ~len:_ ->
              Body.schedule_read
                ~on_read:(fun _ ~off:_ ~len:_ -> ())
                ~on_eof:(fun () ->
                  body_eof_called := true;
                  Reqd.respond_with_string reqd response "")
                request_body)
            ~on_eof:ignore
            request_body)
    in
    let t = create_and_handle_preface ~error_handler request_handler in
    read_request ~body:"request body" t request;
    let data_frame =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.default_flags
          ; frame_type = Data
          }
      ; frame_payload = Frame.Data (Bigstringaf.of_string ~off:0 ~len:3 "foo")
      }
    in
    read_frames t [ data_frame ];
    read_frames
      t
      [ { data_frame with
          frame_header =
            { data_frame.frame_header with
              flags = Flags.(default_flags |> set_end_stream)
            }
        }
      ];
    let window_update_and_response_frames =
      next_write_operation t |> Write_operation.to_write_as_string |> Option.get
    in
    report_write_result
      t
      (`Ok (String.length window_update_and_response_frames));
    writer_yields t;
    Alcotest.(check bool)
      "Response body read handler called"
      true
      !body_read_called;
    Alcotest.(check bool)
      "Response body EOF handler called"
      true
      !body_eof_called

  let test_flow_control () =
    let body_read_called = ref false in
    let request = Request.create ~scheme:"http" `GET "/" in
    let request_handler reqd =
      let request_body = Reqd.request_body reqd in
      Body.schedule_read
        request_body
        ~on_eof:ignore
        ~on_read:(fun _bs ~off:_ ~len:_ -> body_read_called := true)
    in
    let t = create_and_handle_preface ~error_handler request_handler in
    read_request ~body:"request body" t request;
    let data_frame =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.(default_flags |> set_end_stream)
          ; frame_type = Data
          }
      ; frame_payload = Frame.Data (Bigstringaf.of_string ~off:0 ~len:3 "foo")
      }
    in
    read_frames t [ data_frame ];
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      Alcotest.(check (list int))
        "Only writes are WINDOW_UPDATE frames"
        (List.map
           Frame.FrameType.serialize
           Frame.FrameType.[ WindowUpdate; WindowUpdate ])
        (List.map
           (fun Frame.{ frame_header = { frame_type; _ }; _ } ->
             Frame.FrameType.serialize frame_type)
           frames);
      report_write_result t (`Ok (IOVec.lengthv iovecs));
      Alcotest.(check bool) "Response handler called" true !body_read_called;
      writer_yields t
    | _ ->
      assert false

  let test_flow_control_can_send_empty_data_frame () =
    let request = Request.create ~scheme:"http" `GET "/" in
    let request_handler reqd =
      let response = Response.create `OK in
      let response_body = Reqd.respond_with_streaming reqd response in
      Body.write_string response_body "hello";
      Body.flush response_body (fun () -> Body.close_writer response_body)
    in
    let t =
      create_and_handle_preface
        ~settings:Settings.[ InitialWindowSize 5l ]
        ~error_handler
        request_handler
    in
    read_request ~body:"request body" t request;
    let data_frame =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.(default_flags |> set_end_stream)
          ; frame_type = Data
          }
      ; frame_payload = Frame.Data (Bigstringaf.of_string ~off:0 ~len:3 "foo")
      }
    in
    read_frames t [ data_frame ];
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      Alcotest.(check (list int))
        "Response written"
        (List.map Frame.FrameType.serialize Frame.FrameType.[ Headers; Data ])
        (List.map
           (fun Frame.{ frame_header = { frame_type; _ }; _ } ->
             Frame.FrameType.serialize frame_type)
           frames);
      report_write_result t (`Ok (IOVec.lengthv iovecs));
      (match next_write_operation t with
      | `Write iovecs ->
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        Alcotest.(check (list int))
          "Final 0-length DATA frame is not subject to flow control"
          (List.map Frame.FrameType.serialize Frame.FrameType.[ Data ])
          (List.map
             (fun Frame.{ frame_header = { frame_type; _ }; _ } ->
               Frame.FrameType.serialize frame_type)
             frames);
        report_write_result t (`Ok (IOVec.lengthv iovecs));
        writer_yields t
      | _ ->
        assert false)
    | _ ->
      assert false

  let trailers_request_handler reqd =
    let response = Response.create `OK in
    (* Send the response for / *)
    let response_body = Reqd.respond_with_streaming reqd response in
    Body.write_string response_body "somedata";
    Body.flush response_body (fun () ->
        Reqd.schedule_trailers reqd Headers.(add empty "foo" "bar");
        Body.close_writer response_body)

  let test_trailers () =
    let t = create ~error_handler trailers_request_handler in
    handle_preface t;
    let headers, _ = header_and_continuation_frames in
    let headers =
      { headers with
        Frame.frame_header =
          { headers.frame_header with
            flags = Flags.(default_flags |> set_end_header |> set_end_stream)
          }
      }
    in
    read_frames t [ headers ];
    match next_write_operation t with
    | `Write iovecs ->
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      List.iter2
        (fun { Frame.frame_header; _ } (label, frame_type, flags) ->
          Alcotest.(check int)
            ("Next write operation surfaces writes for " ^ label)
            (Frame.FrameType.serialize frame_header.frame_type)
            (Frame.FrameType.serialize frame_type);
          Alcotest.(check int) "Correct flags are used" frame_header.flags flags)
        frames
        Frame.FrameType.
          [ ("HEADERS", Headers, Flags.(set_end_header default_flags))
          ; "DATA", Data, Flags.default_flags
          ];
      let iovec_len = IOVec.lengthv iovecs in
      report_write_result t (`Ok iovec_len);
      (match next_write_operation t with
      | `Write iovecs ->
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
        Alcotest.(check int)
          "Next write operation surfaces the trailers HEADERS frame"
          Frame.FrameType.(serialize Headers)
          Frame.FrameType.(serialize frame.frame_header.frame_type);
        Alcotest.(check int)
          "Last HEADERS frame has END_STREAM and END_HEADERS flag"
          frame.frame_header.flags
          Flags.(set_end_stream (set_end_header default_flags));
        let iovec_len = IOVec.lengthv iovecs in
        report_write_result t (`Ok iovec_len);
        Alcotest.check
          write_operation
          "Writer yields"
          `Yield
          (next_write_operation t);
        Alcotest.check
          read_operation
          "Reader wants to read"
          `Read
          (next_read_operation t)
      | _ ->
        Alcotest.fail
          "Expected state machine to issue a write operation after seeing \
           headers.")
    | _ ->
      Alcotest.fail
        "Expected state machine to issue a write operation after seeing \
         headers."

  (* TODO: test graceful shutdown, allowing lower numbered streams to
     complete. *)
  let suite =
    [ "initial reader state", `Quick, test_initial_reader_state
    ; "shutdown reader closed", `Quick, test_reader_is_closed_after_eof
    ; "malformed frame", `Quick, test_reading_malformed_frame w
    ; ( "malformed frame"
      , `Quick
      , test_reading_malformed_frame ~is_failure:true w2 )
    ; "malformed frame", `Quick, test_reading_just_header w3
    ; ( "send frames after a padded frame"
      , `Quick
      , test_send_frame_after_padded_frame )
    ; ( "continuation frame on the same stream (correct)"
      , `Quick
      , test_continuation_frame )
    ; ( "continuation frame on another stream"
      , `Quick
      , test_continuation_frame_another_stream )
    ; ( "frame size error (frame size exceeds `Config.read_buffer_size`)"
      , `Quick
      , test_read_frame_size_error )
    ; ( "frame size error on a priority frame"
      , `Quick
      , test_read_frame_size_error_priority_frame )
    ; ( "connection preface read with more frames"
      , `Quick
      , test_preface_read_with_more_frames )
    ; ( "settings that exceeds the maximum allowed"
      , `Quick
      , test_settings_frame_unsigned )
    ; "open existing stream", `Quick, test_open_existing_stream
    ; "dependent stream", `Quick, test_dependent_stream
    ; "server push", `Quick, test_server_push
    ; "CONNECT method", `Quick, test_connect
    ; "CONNECT method (malformed)", `Quick, test_connect_malformed
    ; ( "Client sends 0 max concurrent streams"
      , `Quick
      , test_client_max_concurrent_streams )
    ; ( "empty fixed streaming response"
      , `Quick
      , test_empty_fixed_streaming_response )
    ; "starting an h2c connection", `Quick, test_h2c
    ; ( "non-zero `content-length` and no DATA frames"
      , `Quick
      , test_nonzero_content_length_no_data_frames )
    ; "premature remote close with pending bytes", `Quick, test_unexpected_eof
    ; ( "frame size error unknown frame"
      , `Quick
      , test_read_frame_size_error_unknown_frame )
    ; ( "reading the request body as it arrives"
      , `Quick
      , test_reading_request_body )
    ; "flow control", `Quick, test_flow_control
    ; ( "flow control -- can send empty data frame"
      , `Quick
      , test_flow_control_can_send_empty_data_frame )
    ; "trailers", `Quick, test_trailers
    ]
end

let () =
  Alcotest.run
    "ocaml-h2 unit tests"
    [ "server_connection", Server_connection_tests.suite ]
