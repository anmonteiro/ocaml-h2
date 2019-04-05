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
        (* | `Error (Error.ConnectionError (e, msg)) -> Format.sprintf
           "ConnectionError: %ld %S" (Error.serialize e) msg | `Error
           (Error.StreamError (stream_id, e)) -> Format.sprintf "StreamError on
           %ld: %ld" stream_id (Error.serialize e) *)
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

  let encode_headers hpack_encoder headers =
    let f = Faraday.create 0x1000 in
    Serialize.Writer.encode_headers hpack_encoder f headers;
    Faraday.serialize_to_bigstring f

  let preface =
    let writer = Writer.create 0x400 in
    let frame_info = Writer.make_frame_info 0l in
    Writer.write_settings writer frame_info [];
    Faraday.serialize_to_string (Serialize.Writer.faraday writer)

  let handle_preface t =
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

  let test_set_up_connection () =
    let t =
      create
        ?config:None
        ?push_handler:None
        ~error_handler:default_error_handler
    in
    handle_preface t

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
    match next_write_operation t with
    | `Write iovecs ->
      let lenv = IOVec.lengthv iovecs in
      Alcotest.(check bool) "Writev length > 0" true (lenv > 0);
      report_write_result t (`Ok lenv);
      Alcotest.(check write_operation)
        "Writer yields"
        `Yield
        (next_write_operation t);
      Body.close_writer request_body;
      (match next_write_operation t with
      | `Write iovecs ->
        let lenv = IOVec.lengthv iovecs in
        Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
        Alcotest.(check int)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          (Frame.FrameType.serialize Data)
          Frame.(frame.frame_header.frame_type |> FrameType.serialize);
        Alcotest.(check bool)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          true
          (Flags.test_end_stream frame.frame_header.flags);
        report_write_result t (`Ok lenv);
        let hpack_encoder = Hpack.Encoder.create 4096 in
        let headers =
          { Frame.frame_header =
              { payload_length = 0
              ; stream_id = 1l
              ; flags =
                  Flags.(default_flags |> set_end_stream |> set_end_header)
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
        let headers_wire = Test_common.serialize_frame headers in
        let headers_length = Bigstringaf.length headers_wire in
        let read_headers = read t ~off:0 ~len:headers_length headers_wire in
        Alcotest.(check int)
          "Read the entire first frame"
          headers_length
          read_headers;
        Alcotest.(check bool) "Response handler called" true !handler_called
      | `Yield | `Close _ ->
        Alcotest.fail "Expected client connection to issue a `Write operation")
    | `Yield | `Close _ ->
      Alcotest.fail "Expected client connection to issue a `Write operation"

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
    match next_write_operation t with
    | `Write iovecs ->
      let lenv = IOVec.lengthv iovecs in
      Alcotest.(check bool) "Writev length > 0" true (lenv > 0);
      report_write_result t (`Ok lenv);
      Alcotest.(check write_operation)
        "Writer yields"
        `Yield
        (next_write_operation t);
      Body.close_writer request_body;
      (match next_write_operation t with
      | `Write iovecs ->
        let lenv = IOVec.lengthv iovecs in
        Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
        Alcotest.(check int)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          (Frame.FrameType.serialize Data)
          Frame.(frame.frame_header.frame_type |> FrameType.serialize);
        Alcotest.(check bool)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          true
          (Flags.test_end_stream frame.frame_header.flags);
        report_write_result t (`Ok lenv);
        let hpack_encoder = Hpack.Encoder.create 4096 in
        let headers =
          { Frame.frame_header =
              { payload_length = 0
              ; stream_id = 1l
              ; flags = Flags.(default_flags |> set_end_header)
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
        let headers_wire = Test_common.serialize_frame headers in
        let headers_length = Bigstringaf.length headers_wire in
        let read_headers = read t ~off:0 ~len:headers_length headers_wire in
        Alcotest.(check int)
          "Read the entire first frame"
          headers_length
          read_headers;
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
          Writer.make_frame_info
            ~flags:Flags.(default_flags |> set_end_header)
            1l
        in
        Writer.write_push_promise
          writer
          hpack_encoder
          frame_info
          ~promised_id:2l
          request;
        let push_wire =
          Faraday.serialize_to_bigstring (Writer.faraday writer)
        in
        let push_length = Bigstringaf.length push_wire in
        let read_push = read t ~off:0 ~len:push_length push_wire in
        Alcotest.(check int) "Read the entire push frame" push_length read_push;
        Alcotest.(check bool) "Push handler called" true !push_handler_called;
        let response_headers =
          { Frame.frame_header =
              { payload_length = 0
              ; stream_id = 2l
              ; flags =
                  Flags.(default_flags |> set_end_header |> set_end_stream)
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
        let response_headers_wire =
          Test_common.serialize_frame response_headers
        in
        let response_headers_length = Bigstringaf.length headers_wire in
        let read_response_headers =
          read t ~off:0 ~len:response_headers_length response_headers_wire
        in
        Alcotest.(check int)
          "Read the entire response frame"
          response_headers_length
          read_response_headers;
        Alcotest.(check bool)
          "Push response handler called"
          true
          !push_response_handler_called
      | `Yield | `Close _ ->
        Alcotest.fail "Expected client connection to issue a `Write operation")
    | `Yield | `Close _ ->
      Alcotest.fail "Expected client connection to issue a `Write operation"

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
    match next_write_operation t with
    | `Write iovecs ->
      let lenv = IOVec.lengthv iovecs in
      Alcotest.(check bool) "Writev length > 0" true (lenv > 0);
      report_write_result t (`Ok lenv);
      Alcotest.(check write_operation)
        "Writer yields"
        `Yield
        (next_write_operation t);
      Body.close_writer request_body;
      (match next_write_operation t with
      | `Write iovecs ->
        let lenv = IOVec.lengthv iovecs in
        Alcotest.(check int) "Writer issues a zero-payload DATA frame" 9 lenv;
        let frames = parse_frames (Write_operation.iovecs_to_string iovecs) in
        let frame = List.hd frames in
        Alcotest.(check int)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          (Frame.FrameType.serialize Data)
          Frame.(frame.frame_header.frame_type |> FrameType.serialize);
        Alcotest.(check bool)
          "Next write operation is an empty DATA frame with the END_STREAM \
           flag set"
          true
          (Flags.test_end_stream frame.frame_header.flags);
        report_write_result t (`Ok lenv);
        let hpack_encoder = Hpack.Encoder.create 4096 in
        let headers =
          { Frame.frame_header =
              { payload_length = 0
              ; stream_id = 1l
              ; flags = Flags.(default_flags |> set_end_header)
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
        let headers_wire = Test_common.serialize_frame headers in
        let headers_length = Bigstringaf.length headers_wire in
        let read_headers = read t ~off:0 ~len:headers_length headers_wire in
        Alcotest.(check int)
          "Read the entire first frame"
          headers_length
          read_headers;
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
          Writer.make_frame_info
            ~flags:Flags.(default_flags |> set_end_header)
            1l
        in
        Writer.write_push_promise
          writer
          hpack_encoder
          frame_info
          ~promised_id:2l
          request;
        let push_wire =
          Faraday.serialize_to_bigstring (Writer.faraday writer)
        in
        let push_length = Bigstringaf.length push_wire in
        let read_push = read t ~off:0 ~len:push_length push_wire in
        Alcotest.(check int) "Read the entire push frame" push_length read_push;
        Alcotest.(check bool) "Push handler called" true !push_handler_called;
        (match next_write_operation t with
        | `Write iovecs ->
          let frames =
            parse_frames (Write_operation.iovecs_to_string iovecs)
          in
          let frame = List.hd frames in
          Alcotest.(check int)
            "Next write operation is an RST_STREAM frame with the Cancel error"
            (Frame.FrameType.serialize RSTStream)
            Frame.(frame.frame_header.frame_type |> FrameType.serialize);
          Alcotest.(check bool)
            "Next write operation is an RST_STREAM frame with the Cancel error"
            true
            (Frame.RSTStream Error.Cancel = frame.frame_payload)
        | `Yield | `Close _ ->
          Alcotest.fail
            "Expected client connection to issue a `Write operation")
      | `Yield | `Close _ ->
        Alcotest.fail "Expected client connection to issue a `Write operation")
    | `Yield | `Close _ ->
      Alcotest.fail "Expected client connection to issue a `Write operation"

  (* TODO: test continuation frames *)
  (* TODO: test continuation frames on a different streams (error case) *)
  (* TODO: test data larger than content length *)
  (* TODO: test ping, including multiple pings and their order (FIFO) *)
  (* TODO: test stream-level error handler is called *)
  (* TODO: test connection-level error handler is called *)
  let suite =
    [ "initial reader state", `Quick, test_initial_reader_state
    ; "set up client connection", `Quick, test_set_up_connection
    ; "simple GET request", `Quick, test_simple_get_request
    ; "push handler successful response", `Quick, test_push_handler_success
    ; "push handler cancels push", `Quick, test_push_handler_cancel
    ]
end

let () =
  Alcotest.run
    "ocaml-h2 unit tests"
    [ "client_connection", Client_connection_tests.suite ]
