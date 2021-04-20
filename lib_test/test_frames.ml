open H2__
open Test_common
module Json = Yojson.Basic.Util

let fixtures_dir = "http2-frame-test-case"

let success_fixtures, error_fixtures =
  fixtures_dir
  |> Sys.readdir
  |> Array.to_list
  |> List.map (fun dir -> dir, Filename.concat fixtures_dir dir)
  |> List.filter (fun (_, dir) -> Sys.is_directory dir)
  |> List.map (fun (dir, fullpath) ->
         let files_in_dir =
           fullpath
           |> Sys.readdir
           |> Array.to_list
           |> List.map (fun file -> Filename.concat fullpath file)
           |> List.filter (fun file ->
                  (not (Sys.is_directory file))
                  && Filename.extension file = ".json")
         in
         dir, files_in_dir)
  |> List.partition (fun (dir, _) -> dir <> "error")

module P = struct
  open Parse

  let parse_fn wire success_handler error_handler =
    let handler = function
      | Ok frame ->
        success_handler frame
      | Error e ->
        error_handler e
    in
    let reader =
      Reader.server_frames
        ~max_frame_size:Settings.default.max_frame_size
        (fun _ -> ignore)
        handler
    in
    handle_preface reader;
    let wire_bs = wire |> string_of_hex |> bs_of_string in
    let _read =
      Reader.read_with_more
        reader
        wire_bs
        ~off:0
        ~len:(Bigstringaf.length wire_bs)
        Incomplete
    in
    reader

  let parse_success wire handler =
    let reader =
      parse_fn wire handler (fun _ ->
          Alcotest.(fail "Expected to have thrown an error parsing frame."))
    in
    match Reader.next reader with
    | `Read ->
      ()
    | `Close | `Error _ ->
      Alcotest.(fail "Expected to have sucessfully parsed frame.")

  let parse_error wire handler =
    let reader =
      parse_fn
        wire
        (fun _ ->
          Alcotest.(fail "Expected to have thrown an error parsing frame."))
        handler
    in
    match Reader.next reader with
    | `Read ->
      ()
    | `Close ->
      Alcotest.(fail "Expected to have thrown an error parsing frame.")
    | `Error e ->
      handler e

  let serialize ?padding frame =
    let output = Test_common.serialize_frame_string ?padding frame in
    hex_of_string output
end

let frame_testable =
  (module struct
    type t = Frame.t

    let priority_to_yojson priority =
      if priority != Priority.default_priority then
        let { Priority.exclusive; stream_dependency; weight } = priority in
        [ "stream_dependency", `Int (Int32.to_int stream_dependency)
        ; "weight", `Int weight
        ; "exclusive", `Bool exclusive
        ]
      else
        [ "stream_dependency", `Null; "weight", `Null; "exclusive", `Null ]

    let bs_to_string bs =
      let off = 0 in
      let len = Bigstringaf.length bs in
      Bigstringaf.substring ~off ~len bs

    let frame_payload_to_json frame_type payload =
      let others =
        match payload with
        | Frame.Data data ->
          [ "data", `String (bs_to_string data) ]
        | Headers (priority, fragment) ->
          ("header_block_fragment", `String (bs_to_string fragment))
          :: priority_to_yojson priority
        | Priority priority ->
          priority_to_yojson priority
        | RSTStream error_code ->
          [ "error_code", `Int (Error_code.serialize error_code |> Int32.to_int)
          ]
        | Settings settings_list ->
          [ ( "settings"
            , `List
                (List.map
                   (fun setting ->
                     `List
                       [ `Int (Settings.serialize_key setting)
                       ; `Int
                           (match setting with
                           | MaxConcurrentStreams value
                           | InitialWindowSize value ->
                             Int32.to_int value
                           | HeaderTableSize value
                           | EnablePush value
                           | MaxFrameSize value
                           | MaxHeaderListSize value ->
                             value)
                       ])
                   settings_list) )
          ]
        | PushPromise (stream_identifier, fragment) ->
          [ "header_block_fragment", `String (bs_to_string fragment)
          ; "promised_stream_id", `Int (Int32.to_int stream_identifier)
          ]
        | Ping data ->
          [ "opaque_data", `String (bs_to_string data) ]
        | GoAway (stream_identifier, error_code, debug_data) ->
          [ "error_code", `Int (Error_code.serialize error_code |> Int32.to_int)
          ; "additional_debug_data", `String (bs_to_string debug_data)
          ; "last_stream_id", `Int (Int32.to_int stream_identifier)
          ]
        | WindowUpdate increment ->
          [ "window_size_increment", `Int (Int32.to_int increment) ]
        | Continuation fragment ->
          [ "header_block_fragment", `String (bs_to_string fragment) ]
        | Unknown (_frame_type, _) ->
          assert false
      in
      `Assoc (("type", `Int (Frame.FrameType.serialize frame_type)) :: others)

    let frame_to_yojson { Frame.frame_header; frame_payload } =
      let { Frame.payload_length; flags; stream_id; frame_type } =
        frame_header
      in
      `Assoc
        [ "length", `Int payload_length
        ; "flags", `Int flags
        ; "stream_identifier", `Int (Int32.to_int stream_id)
        ; "frame_payload", frame_payload_to_json frame_type frame_payload
        ]

    let pp formatter t =
      Format.pp_print_text
        formatter
        (t |> frame_to_yojson |> Yojson.Safe.pretty_to_string)

    let equal = ( = )
  end : Alcotest.TESTABLE
    with type t = Frame.t)

let priority_of_json json =
  let stream_dependency =
    Json.(json |> member "stream_dependency" |> to_int_option)
  in
  let weight = Json.(json |> member "weight" |> to_int_option) in
  let exclusive = Json.(json |> member "exclusive" |> to_bool_option) in
  match stream_dependency, weight, exclusive with
  | Some stream_dependency, Some weight, Some exclusive ->
    { Priority.exclusive
    ; stream_dependency = Int32.of_int stream_dependency
    ; weight
    }
  | _ ->
    Priority.default_priority

let frame_type_of_string = function
  | "data" ->
    Frame.FrameType.Data
  | "headers" ->
    Headers
  | "priority" ->
    Priority
  | "rst_stream" ->
    RSTStream
  | "settings" ->
    Settings
  | "push_promise" ->
    PushPromise
  | "ping" ->
    Ping
  | "goaway" ->
    GoAway
  | "window_update" ->
    WindowUpdate
  | "continuation" ->
    Continuation
  | _ ->
    assert false

let frame_payload_of_json frame_type json =
  match frame_type with
  | Frame.FrameType.Data ->
    Frame.Data Json.(json |> member "data" |> to_string |> bs_of_string)
  | Headers ->
    let priority = priority_of_json json in
    let fragment =
      Json.(json |> member "header_block_fragment" |> to_string |> bs_of_string)
    in
    Headers (priority, fragment)
  | Priority ->
    Priority (priority_of_json json)
  | RSTStream ->
    let error_code =
      Json.(json |> member "error_code" |> to_int |> Int32.of_int)
    in
    RSTStream (Error_code.parse error_code)
  | Settings ->
    let settings =
      List.map
        (fun setting_json ->
          let setting = Json.to_list setting_json in
          let key_value = List.nth setting 1 |> Json.to_int in
          match Json.to_int (List.hd setting) with
          | 0x1 ->
            Settings.HeaderTableSize key_value
          | 0x2 ->
            EnablePush key_value
          | 0x3 ->
            MaxConcurrentStreams (Int32.of_int key_value)
          | 0x4 ->
            InitialWindowSize (Int32.of_int key_value)
          | 0x5 ->
            MaxFrameSize key_value
          | 0x6 ->
            MaxHeaderListSize key_value
          | _ ->
            raise (Invalid_argument "settings key id"))
        Json.(json |> member "settings" |> to_list)
    in
    Settings settings
  | PushPromise ->
    let fragment =
      Json.(json |> member "header_block_fragment" |> to_string |> bs_of_string)
    in
    let promised_stream_id =
      Json.(json |> member "promised_stream_id" |> to_int |> Int32.of_int)
    in
    PushPromise (promised_stream_id, fragment)
  | Ping ->
    let opaque_data =
      Json.(json |> member "opaque_data" |> to_string |> bs_of_string)
    in
    Ping opaque_data
  | GoAway ->
    let error_code =
      Json.(json |> member "error_code" |> to_int |> Int32.of_int)
    in
    let last_stream_id =
      Json.(json |> member "last_stream_id" |> to_int |> Int32.of_int)
    in
    let debug_data =
      Json.(json |> member "additional_debug_data" |> to_string |> bs_of_string)
    in
    GoAway (last_stream_id, Error_code.parse error_code, debug_data)
  | WindowUpdate ->
    let window_size_increment =
      Json.(json |> member "window_size_increment" |> to_int) |> Int32.of_int
    in
    WindowUpdate window_size_increment
  | Continuation ->
    let fragment =
      Json.(json |> member "header_block_fragment" |> to_string |> bs_of_string)
    in
    Continuation fragment
  | Unknown _ ->
    assert false

let success_suites =
  let gen_suite ~frame_type filename =
    let fixture = read_all filename |> Yojson.Basic.from_string in
    let test_case_name = Json.(fixture |> member "description" |> to_string) in
    ( Printf.sprintf "%s: %s" filename test_case_name
    , `Quick
    , fun () ->
        let expected_frame_json = Json.(member "frame" fixture) in
        let frame_payload_json =
          Json.(expected_frame_json |> member "frame_payload")
        in
        let frame_type = frame_type_of_string frame_type in
        let expected_frame =
          { Frame.frame_header =
              { payload_length =
                  Json.(expected_frame_json |> member "length" |> to_int)
              ; flags = Json.(expected_frame_json |> member "flags" |> to_int)
              ; stream_id =
                  Json.(
                    expected_frame_json
                    |> member "stream_identifier"
                    |> to_int
                    |> Int32.of_int)
              ; frame_type
              }
          ; frame_payload = frame_payload_of_json frame_type frame_payload_json
          }
        in
        let wire = Json.(fixture |> member "wire" |> to_string) in
        P.parse_success wire (fun frame ->
            Alcotest.check
              frame_testable
              "Frames are equal"
              expected_frame
              frame;
            let padding =
              match
                Json.(
                  frame_payload_json |> member "padding" |> to_string_option)
              with
              | Some padding ->
                Some
                  Bigstringaf.(
                    of_string ~off:0 ~len:(String.length padding) padding)
              | None ->
                None
            in
            let serialized_wire = P.serialize ?padding frame in
            Alcotest.(
              check
                string
                "Parse / Serialize roundtripping"
                wire
                serialized_wire)) )
  in
  List.map
    (fun (name, files) ->
      let suite_name = Printf.sprintf "%s frame" name in
      suite_name, List.map (gen_suite ~frame_type:name) files)
    success_fixtures

let error_suites =
  let gen_suite filename =
    let fixture = read_all filename |> Yojson.Basic.from_string in
    let test_case_name = Json.(fixture |> member "description" |> to_string) in
    ( Printf.sprintf "%s: %s" filename test_case_name
    , `Quick
    , fun () ->
        let possible_errors = Json.(member "error" fixture |> to_list) in
        let expected_error =
          possible_errors |> List.hd |> Json.to_int |> Int32.of_int
        in
        let wire = Json.(fixture |> member "wire" |> to_string) in
        P.parse_error wire (function
            | Error.ConnectionError (e, _) | Error.StreamError (_, e) ->
            Alcotest.(check int32)
              "Expected Error Code"
              expected_error
              (Error_code.serialize e)) )
  in
  List.map
    (fun (_, files) -> "Error tests", List.map gen_suite files)
    error_fixtures

let () = Alcotest.run "lambda-runtime" (success_suites @ error_suites)
