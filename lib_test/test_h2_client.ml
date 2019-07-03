open Test_common
open H2
open H2__
module Writer = Serialize.Writer

module Client_connection_tests = struct
  open Client_connection

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
          Format.sprintf "ConnectionError: %ld %S" (Error.serialize e) msg
        | `Error (Error.StreamError (stream_id, e)) ->
          Format.sprintf
            "StreamError on %ld: %ld"
            stream_id
            (Error.serialize e)
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

  let default_error_handler _ = assert false

  let test_initial_reader_state () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    Alcotest.(check read_operation)
      "A new reader wants input"
      `Read
      (next_read_operation t)

  let test_reader_is_closed_after_eof () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation)
      "Shutting down a reader closes it"
      `Close
      (next_read_operation t);
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    let c = read t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read with no input returns 0" 0 c;
    let c = read_eof t Bigstringaf.empty ~off:0 ~len:0 in
    Alcotest.(check int) "read_eof with no input returns 0" 0 c;
    Alcotest.(check read_operation)
      "Shutting down a reader closes it"
      `Close
      (next_read_operation t)

  let preface =
    let writer = Writer.create 0x400 in
    let frame_info = Writer.make_frame_info 0l in
    Writer.write_settings writer frame_info [];
    Faraday.serialize_to_string (Serialize.Writer.faraday writer)

  let read_frames conn frames =
    List.iter
      (fun frame ->
        let frame_wire = Test_common.serialize_frame frame in
        let frame_length = Bigstringaf.length frame_wire in
        let read_frame = read conn ~off:0 ~len:frame_length frame_wire in
        Alcotest.(check int) "Read the entire frame" frame_length read_frame)
      frames

  let write_response
      t
      hpack_encoder
      ?priority
      ?(stream_id = 1l)
      ?(flags = Flags.(default_flags |> set_end_stream |> set_end_header))
      response
    =
    let writer = Writer.create 4096 in
    let frame_info = Writer.make_frame_info ~flags stream_id in
    Writer.write_response_headers
      ?priority
      writer
      hpack_encoder
      frame_info
      response;
    let headers_wire =
      Faraday.serialize_to_bigstring (Writer.faraday writer)
    in
    let headers_length = Bigstringaf.length headers_wire in
    let read_headers = read t ~off:0 ~len:headers_length headers_wire in
    Alcotest.(check int)
      "Read the entire HEADERS frame of the response"
      headers_length
      read_headers

  let write_response_body
      t ?(stream_id = 1l) ?(flags = Flags.(default_flags |> set_end_stream)) s
    =
    let writer = Writer.create 4096 in
    let frame_info = Writer.make_frame_info ~flags stream_id in
    Writer.write_data writer frame_info s;
    let data_wire = Faraday.serialize_to_bigstring (Writer.faraday writer) in
    let data_length = Bigstringaf.length data_wire in
    let read_data = read t ~off:0 ~len:data_length data_wire in
    Alcotest.(check int)
      "Read the entire DATA frame of the response body"
      data_length
      read_data

  let flush_pending_writes t =
    match next_write_operation t with
    | `Write iovecs ->
      let lenv = IOVec.lengthv iovecs in
      Alcotest.(check bool) "Writev length > 0" true (lenv > 0);
      let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
      frames, lenv
    | `Yield | `Close _ ->
      Alcotest.fail "Expected client connection to issue a `Write operation"

  let flush_request t =
    let _, lenv = flush_pending_writes t in
    report_write_result t (`Ok lenv);
    Alcotest.(check write_operation)
      "Writer yields"
      `Yield
      (next_write_operation t)

  (* Well-formed HEADERS + CONTINUATION frames. *)
  let header_and_continuation_frames =
    let hpack_encoder = Hpack.Encoder.create () in
    let headers =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.(set_end_stream default_flags)
          ; frame_type = Headers
          }
      ; frame_payload =
          Frame.Headers
            ( None
            , encode_headers
                hpack_encoder
                Headers.(of_list [ ":status", "200" ]) )
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

  let handle_preface t =
    let preface_len = String.length preface in
    let preface = read t (bs_of_string preface) ~off:0 ~len:preface_len in
    Alcotest.(check int)
      "read preface returns preface length"
      preface_len
      preface;
    let _, lenv = flush_pending_writes t in
    report_write_result t (`Ok lenv)

  let test_set_up_connection () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t

  let test_invalid_preface () =
    let error_handler_called = ref false in
    let error_handler error =
      error_handler_called := true;
      match error with
      | `Protocol_error ->
        Alcotest.(check pass)
          "Error handler is called with a protocol error"
          ()
          ()
      | _ ->
        Alcotest.fail "Expected error handler to be called with protocol error"
    in
    let t = create ?config:None ?push_handler:None ~error_handler in
    let _, lenv = flush_pending_writes t in
    report_write_result t (`Ok lenv);
    let headers, _ = header_and_continuation_frames in
    read_frames t [ headers ];
    Alcotest.check
      read_operation
      "There was a connection error of type PROTOCOL_ERROR"
      (`Error
        Error.(ConnectionError (ProtocolError, "Invalid connection preface")))
      (Reader.next t.reader);
    Alcotest.check
      read_operation
      "Reader issues a `Close operation"
      `Close
      (next_read_operation t);
    Alcotest.(check bool) "Error handler got called" true !error_handler_called

  let test_simple_get_request () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let hpack_encoder = Hpack.Encoder.create () in
    write_response t hpack_encoder (Response.create `OK);
    Alcotest.(check bool) "Response handler called" true !handler_called

  let test_data_larger_than_reported () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let response_handler _response _response_body = () in
    let error_handler_called = ref false in
    let stream_level_error_handler error =
      error_handler_called := true;
      match error with
      | `Invalid_response_body_length _response ->
        Alcotest.(check pass)
          "Stream error handler gets an invalid response body length"
          true
          true
      | _ ->
        Alcotest.fail "Expected stream error handler to pass"
    in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:stream_level_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let hpack_encoder = Hpack.Encoder.create () in
    write_response
      t
      hpack_encoder
      ~flags:Flags.(default_flags |> set_end_header)
      (Response.create `OK ~headers:(Headers.of_list [ "content-length", "2" ]));
    write_response_body t "foo";
    Alcotest.(check bool)
      "Stream level error handler called"
      true
      !error_handler_called

  let test_stream_level_protocol_error () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let response_handler _response _response_body = () in
    let error_handler_called = ref false in
    let stream_level_error_handler error =
      error_handler_called := true;
      match error with
      | `Protocol_error ->
        Alcotest.(check pass)
          "Stream error handler gets a protocol error"
          ()
          ()
      | _ ->
        Alcotest.fail "Expected stream error handler to pass"
    in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:stream_level_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let hpack_encoder = Hpack.Encoder.create () in
    write_response
      t
      hpack_encoder
      ~priority:
        (* depend on itself *)
        { Priority.default_priority with stream_dependency = 1l }
      (Response.create `OK ~headers:(Headers.of_list [ "content-length", "2" ]));
    Alcotest.(check bool)
      "Stream level error handler called"
      true
      !error_handler_called

  let test_continuation_frame () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    let second_handler_called = ref false in
    let second_response_handler _response _response_body =
      second_handler_called := true
    in
    let second_request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler:second_response_handler
    in
    flush_request t;
    Body.close_writer second_request_body;
    let _, _ = flush_pending_writes t in
    let headers, continuation = header_and_continuation_frames in
    read_frames t [ headers; continuation ];
    Alcotest.(check bool) "Response handler called" true !handler_called;
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
    Alcotest.(check bool)
      "Second Response handler called"
      true
      !second_handler_called

  let test_continuation_frame_other_stream () =
    let error_handler_called = ref false in
    let error_handler _ = error_handler_called := true in
    let t = create ?config:None ?push_handler:None ~error_handler in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let headers, continuation = header_and_continuation_frames in
    let continuation =
      { continuation (* continuation header on a different stream *) with
        frame_header = { continuation.frame_header with stream_id = 3l }
      }
    in
    read_frames t [ headers; continuation ];
    Alcotest.(check bool) "Error handler called" true !error_handler_called;
    let frames, _ = flush_pending_writes t in
    let frame = List.hd frames in
    Alcotest.(check bool)
      "Issues a GoAway frame of type ProtocolError"
      true
      Frame.FrameType.(
        serialize frame.frame_header.frame_type = serialize GoAway)

  let test_push_handler_success () =
    let push_handler_called = ref false in
    let push_response_handler_called = ref false in
    let push_handler _request =
      push_handler_called := true;
      Ok (fun _response _response_body -> push_response_handler_called := true)
    in
    let t =
      create ?config:None ~push_handler ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let hpack_encoder = Hpack.Encoder.create () in
    write_response
      t
      hpack_encoder
      ~flags:Flags.(default_flags |> set_end_header)
      (Response.create `OK);
    Alcotest.(check bool) "Response handler called" true !handler_called;
    let request =
      Request.create
        ~scheme:"http"
        `GET
        "/"
        ~headers:(Headers.of_list [ ":authority", "foo.com" ])
    in
    let writer = Writer.create 4096 in
    let frame_info =
      Writer.make_frame_info ~flags:Flags.(default_flags |> set_end_header) 1l
    in
    Writer.write_push_promise
      writer
      hpack_encoder
      frame_info
      ~promised_id:2l
      request;
    let push_wire = Faraday.serialize_to_bigstring (Writer.faraday writer) in
    let push_length = Bigstringaf.length push_wire in
    let read_push = read t ~off:0 ~len:push_length push_wire in
    Alcotest.(check int) "Read the entire push frame" push_length read_push;
    Alcotest.(check bool) "Push handler called" true !push_handler_called;
    write_response
      t
      hpack_encoder
      ~stream_id:2l
      ~flags:Flags.(default_flags |> set_end_header |> set_end_stream)
      (Response.create `OK);
    Alcotest.(check bool)
      "Push response handler called"
      true
      !push_response_handler_called

  let test_push_handler_cancel () =
    let push_handler_called = ref false in
    let push_handler _request =
      push_handler_called := true;
      Error ()
    in
    let t =
      create ?config:None ~push_handler ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let hpack_encoder = Hpack.Encoder.create () in
    write_response
      t
      hpack_encoder
      ~flags:Flags.(default_flags |> set_end_header)
      (Response.create `OK);
    Alcotest.(check bool) "Response handler called" true !handler_called;
    let request =
      Request.create
        ~scheme:"http"
        `GET
        "/"
        ~headers:(Headers.of_list [ ":authority", "foo.com" ])
    in
    let writer = Writer.create 4096 in
    let frame_info =
      Writer.make_frame_info ~flags:Flags.(default_flags |> set_end_header) 1l
    in
    Writer.write_push_promise
      writer
      hpack_encoder
      frame_info
      ~promised_id:2l
      request;
    let push_wire = Faraday.serialize_to_bigstring (Writer.faraday writer) in
    let push_length = Bigstringaf.length push_wire in
    let read_push = read t ~off:0 ~len:push_length push_wire in
    Alcotest.(check int) "Read the entire push frame" push_length read_push;
    Alcotest.(check bool) "Push handler called" true !push_handler_called;
    let frames, _ = flush_pending_writes t in
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an RST_STREAM frame with the Cancel error"
      (Frame.FrameType.serialize RSTStream)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an RST_STREAM frame with the Cancel error"
      true
      (Frame.RSTStream Error.Cancel = frame.frame_payload)

  let test_stream_error_on_idle_stream () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let error_handler_called = ref false in
    let stream_level_error_handler error =
      error_handler_called := true;
      match error with
      | `Protocol_error ->
        Alcotest.(check pass)
          "Stream error handler gets a protocol error"
          ()
          ()
      | _ ->
        Alcotest.fail "Expected stream error handler to pass"
    in
    let response_handler _response _response_body = () in
    let _request_body =
      Client_connection.request
        t
        request
        ~error_handler:stream_level_error_handler
        ~response_handler
    in
    let frame_info = Writer.make_frame_info 1l in
    (* depend on itself *)
    let priority = { Priority.default_priority with stream_dependency = 1l } in
    let writer = Writer.create 256 in
    Writer.write_priority writer frame_info priority;
    let wire = Faraday.serialize_to_bigstring (Writer.faraday writer) in
    let wire_length = Bigstringaf.length wire in
    let read_priority = read t ~off:0 ~len:wire_length wire in
    Alcotest.(check int)
      "Read the entire PRIORITY frame"
      wire_length
      read_priority;
    Alcotest.(check bool)
      "Stream error handler called"
      true
      !error_handler_called

  let test_stream_transitions_state () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let handler_called = ref false in
    let response_handler _response _response_body = handler_called := true in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:default_error_handler
        ~response_handler
    in
    flush_request t;
    let stream = opt_exn (Scheduler.find t.streams 1l) in
    Alcotest.(check bool)
      "Stream is in the open state"
      true
      (Stream.is_open stream);
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    Alcotest.(check bool)
      "Stream is in the half-closed (local) state after closing the request \
       body"
      true
      (not (Stream.is_open stream));
    let hpack_encoder = Hpack.Encoder.create () in
    write_response t hpack_encoder (Response.create `OK);
    Alcotest.(check bool) "Response handler called" true !handler_called;
    Alcotest.(check bool)
      "Stream transitions to the closed state once the response has been \
       received"
      true
      (match stream.Stream.state with Closed _ -> true | _ -> false)

  let test_ping () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let ping_handler1_called = ref false in
    let ping_handler2_called = ref false in
    let ping_handler ref () = ref := true in
    ping t (ping_handler ping_handler1_called);
    ping t (ping_handler ping_handler2_called);
    let writer = Writer.create 256 in
    let frame_info =
      Writer.make_frame_info ~flags:Flags.(set_ack default_flags) 0l
    in
    Writer.write_ping writer frame_info Serialize.default_ping_payload;
    let ping_wire = Faraday.serialize_to_bigstring (Writer.faraday writer) in
    let ping_length = Bigstringaf.length ping_wire in
    let read_ping = read t ~off:0 ~len:ping_length ping_wire in
    Alcotest.(check int)
      "Read the entire ping frame of the response"
      ping_length
      read_ping;
    Alcotest.(check bool)
      "First ping handler called"
      true
      !ping_handler1_called;
    Alcotest.(check bool)
      "Only the first ping handler called"
      false
      !ping_handler2_called;
    let read_ping = read t ~off:0 ~len:ping_length ping_wire in
    Alcotest.(check int)
      "Read the entire ping frame of the response"
      ping_length
      read_ping;
    Alcotest.(check bool)
      "Second ping handler called"
      true
      !ping_handler2_called

  let test_error_handler_rst_stream () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t;
    let request = Request.create ~scheme:"http" `GET "/" in
    let response_handler _response _response_body = () in
    let error_handler_called = ref false in
    let stream_level_error_handler error =
      error_handler_called := true;
      match error with
      | `Protocol_error ->
        Alcotest.(check pass)
          "Stream error handler gets a protocol error"
          ()
          ()
      | _ ->
        Alcotest.fail "Expected stream error handler to pass"
    in
    let request_body =
      Client_connection.request
        t
        request
        ~error_handler:stream_level_error_handler
        ~response_handler
    in
    flush_request t;
    Body.close_writer request_body;
    let frames, lenv = flush_pending_writes t in
    Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
    let frame = List.hd frames in
    Alcotest.(check int)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      (Frame.FrameType.serialize Data)
      Frame.(frame.frame_header.frame_type |> FrameType.serialize);
    Alcotest.(check bool)
      "Next write operation is an empty DATA frame with the END_STREAM flag set"
      true
      (Flags.test_end_stream frame.frame_header.flags);
    report_write_result t (`Ok lenv);
    let rst_stream =
      { Frame.frame_header =
          { payload_length = 0
          ; stream_id = 1l
          ; flags = Flags.default_flags
          ; frame_type = RSTStream
          }
      ; frame_payload = Frame.RSTStream Error.ProtocolError
      }
    in
    read_frames t [ rst_stream ];
    Alcotest.(check bool)
      "Stream level error handler called"
      true
      !error_handler_called;

    (* Don't loop *)
    Alcotest.(check write_operation)
      "Writer yields, i.e. don't send an RST_STREAM frame in response to one"
      `Yield
      (next_write_operation t)

  let suite =
    [ "initial reader state", `Quick, test_initial_reader_state
    ; "set up client connection", `Quick, test_set_up_connection
    ; ( "invalid connection preface from the server"
      , `Quick
      , test_invalid_preface )
    ; "simple GET request", `Quick, test_simple_get_request
    ; "data larger than declared", `Quick, test_data_larger_than_reported
    ; ( "stream error handler gets called on stream-level protocol errors"
      , `Quick
      , test_stream_level_protocol_error )
    ; "stream error on idle stream", `Quick, test_stream_error_on_idle_stream
    ; "continuation frame (success)", `Quick, test_continuation_frame
    ; ( "stream correctly transitions state"
      , `Quick
      , test_stream_transitions_state )
    ; ( "continuation frame on another stream"
      , `Quick
      , test_continuation_frame_other_stream )
    ; "push handler successful response", `Quick, test_push_handler_success
    ; "push handler cancels push", `Quick, test_push_handler_cancel
    ; "ping", `Quick, test_ping
    ; ( "stream level error handler called on RST_STREAM frames"
      , `Quick
      , test_error_handler_rst_stream )
    ]
end

let () =
  Alcotest.run
    "ocaml-h2 unit tests"
    [ "client_connection", Client_connection_tests.suite ]
