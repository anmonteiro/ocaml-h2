open Hpack
module Json = Yojson.Basic.Util

let parse_file file =
  let json = Yojson.Basic.from_file file in
  let description =
    match Json.(json |> member "description" |> to_string_option) with
    | Some x ->
      x
    | None ->
      file
  in
  let cases =
    List.map
      (fun case ->
        let header_table_size =
          match Json.(case |> member "header_table_size" |> to_int_option) with
          | Some size ->
            size
          | None ->
            4096
        in
        let wire =
          match Json.(case |> member "wire" |> to_string_option) with
          | Some hex ->
            Hex.to_string (`Hex hex)
          | None ->
            ""
        in
        let headers =
          List.map
            (function
              | `Assoc [ (name, `String value) ] ->
                { Hpack.name; value; sensitive = false }
              | _ ->
                assert false)
            Json.(case |> member "headers" |> to_list)
        in
        header_table_size, wire, headers)
      Json.(json |> member "cases" |> to_list)
  in
  description, cases

let h x = Hex.to_string (`Hex x)

let hex_of_string s = s |> Hex.of_string |> Hex.show

let encode_headers encoder headers =
  let faraday = Faraday.create 0x1000 in
  List.iter (Encoder.encode_header encoder faraday) headers;
  Faraday.serialize_to_string faraday

let encode cases =
  let encoder = Hpack.Encoder.create 4096 in
  List.mapi
    (fun seq (_nosize, _nowire, headers) ->
      let wire = encode_headers encoder headers in
      seq, hex_of_string wire, headers)
    cases

let encode_file fixtures_dir (story, file) =
  let _, cases = parse_file file in
  let result = encode cases in
  let json =
    `Assoc
      [ "description", `String "Encoded by h2's HPACK implementation"
      ; ( "cases"
        , `List
            (result
            |> List.map @@ fun (seq, wire, headers) ->
               `Assoc
                 [ "seqno", `Int seq
                 ; "wire", `String wire
                 ; ( "headers"
                   , `List
                       (headers
                       |> List.map @@ fun { name; value; _ } ->
                          `Assoc [ name, `String value ]) )
                 ]) )
      ]
  in
  let channel =
    open_out Filename.(concat fixtures_dir (concat "ocaml-hpack" story))
  in
  Yojson.pretty_to_channel channel json;
  close_out channel

let encode_raw_data fixtures_dir files =
  List.iter (encode_file fixtures_dir) files

let header_equal { name; value; _ } { name = name'; value = value'; _ } =
  name = name' && value = value'

let header_testable =
  (module struct
    type t = header

    let pp formatter { name; value; _ } = Fmt.pf formatter "%s: %s" name value

    let _pp_with_index formatter { name; value; sensitive } =
      Fmt.pf formatter "%s: %s (%B)" name value sensitive

    let equal h1 h2 = header_equal h1 h2
  end : Alcotest.TESTABLE
    with type t = header)

let headers_list_pp =
  let (module Headers) = header_testable in
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.pp_print_string fmt ";\n")
    Headers.pp

let decode_headers decoder size wire =
  let parser = Angstrom.Buffered.parse (Decoder.decode_headers decoder) in
  match Decoder.set_capacity decoder size with
  | Error _ ->
    assert false
  | Ok () ->
    let state = Angstrom.Buffered.feed parser (`String wire) in
    let state' = Angstrom.Buffered.feed state `Eof in
    (match Angstrom.Buffered.state_to_option state' with
    | Some (Ok headers) ->
      List.rev headers
    | Some _ | None ->
      assert false)

let decode cases =
  let encoder = Encoder.create 4096 in
  (* Note: Encoders and decoders are stateful. To check rountripping we need to
     use 2 decoders. Here's why: We initially decode the headers we parsed from
     JSON and assert that they're the same as the JSON we got.

     We then want to: 1. Encode the resulting headers 2. Decode them again 3.
     Check they are indeed the same

     The reason why we need 2 decoders is because of `1.` above. Since
     compression / decompression state is stateful, and we don't have access to
     the first encoder (which may have state about indexed fields that
     `decoder1` has computed in the meantime), using `decoder1` would produce
     wrong headers (given wrong expectations about indexed header fields).

     From then on, we can feel free to use the `encoder` / `decoder2` pair, as
     that effectively mimics the same "connection". In fact, encoding and
     decoding the same headers multiple times will make the compressed payload
     smaller. We check that too. *)
  let decoder1 = Decoder.create 65536 in
  let decoder2 = Decoder.create 65536 in
  List.iter
    (fun (size, wire, headers) ->
      Encoder.set_capacity encoder size;
      let decoded_headers = decode_headers decoder1 size wire in
      Alcotest.(check int)
        "same length"
        (List.length headers)
        (List.length decoded_headers);
      List.iter2
        (fun h1 h2 ->
          Alcotest.(check header_testable "Headers are decoded correctly" h1 h2))
        headers
        decoded_headers;
      (* roundtripping *)
      let encoded = encode_headers encoder decoded_headers in
      let decoded_headers' = decode_headers decoder2 size encoded in
      Alcotest.(check int)
        "same length"
        (List.length headers)
        (List.length decoded_headers);
      List.iter2
        (fun h1 h2 ->
          Alcotest.(check header_testable "Headers are decoded correctly" h1 h2))
        decoded_headers'
        decoded_headers;
      (* Now check that the `encoded_again` payload is smaller than the `encoded`
       * payload. Indexing has happened! *)
      let enc', dec' =
        Array.fold_left
          (fun (_, decoded_headers) _ ->
            let encoded_again = encode_headers encoder decoded_headers in
            let decoded_again = decode_headers decoder2 size encoded_again in
            encoded_again, decoded_again)
          ("", decoded_headers')
          (Array.make 5 0)
      in
      Alcotest.(check bool)
        "encoded_again payload is smaller or equal than encoded"
        true
        (String.length enc' <= String.length encoded);
      (* And check roundtripping again for good measure. *)
      List.iter2
        (fun h1 h2 ->
          Alcotest.(check header_testable "Headers are decoded correctly" h1 h2))
        dec'
        headers)
    cases

let rec take_n acc i ys =
  match i, ys with
  | 0, _ ->
    acc
  | _, [] ->
    acc
  | _, x :: xs when i > 0 ->
    take_n (x :: acc) (i - 1) xs
  | _ ->
    acc

let gen_suites fixtures =
  let gen_suite filename =
    let test_case_name, fixture = parse_file filename in
    test_case_name, `Slow, fun () -> decode fixture
  in
  List.map
    (fun (suite_name, files) ->
      let suite = List.map gen_suite files in
      suite_name, suite)
    fixtures

let files_in_dir dir = dir |> Sys.readdir |> Array.to_list

let read_fixtures fixtures_dir =
  fixtures_dir
  |> files_in_dir
  |> List.map (fun dir -> dir, Filename.concat fixtures_dir dir)
  (* don't need to decode raw-data, it's already in ocaml-hpack. *)
  |> List.filter (fun (dir, fullpath) ->
         Sys.is_directory fullpath && dir <> "raw-data")
  |> List.map (fun (dir, fullpath) ->
         let files_in_dir =
           fullpath
           |> files_in_dir
           |> List.map (fun file -> Filename.concat fullpath file)
           |> List.filter (fun file ->
                  (not (Sys.is_directory file))
                  && Filename.extension file = ".json")
         in
         dir, files_in_dir)

let test_evicting_table_size_0 () =
  let hs =
    [ { name = ":method"; value = "GET"; sensitive = false }
    ; { name = "field_not_indexed"; value = "foo"; sensitive = false }
    ]
  in
  let encoder = Encoder.create 0 in
  let encoded_headers = encode_headers encoder hs in
  Alcotest.(check bool)
    "Encodes to non-zero hex"
    true
    (String.length encoded_headers > 0);
  (* From RFC7541§6.3: Dynamic Table Size Update
   *   A dynamic table size update signals a change to the size of the dynamic
   *   table.
   *   A dynamic table size update starts with the '001' 3-bit pattern
   *
   * Note: we add 0x20 at the beginning of the following wire to signal a
   * dynamic table size update of 0 before the remaining headers are
   * decoded. *)
  let wire = h ("20" ^ hex_of_string encoded_headers) in
  let decoder = Decoder.create 4096 in
  let decoded_headers = decode_headers decoder 4096 wire in
  List.iter2
    (fun h1 h2 ->
      Alcotest.(check header_testable "Decoded headers are roundtripped" h1 h2))
    hs
    decoded_headers

let test_evicting_table_size_0_followup () =
  let hs =
    [ { name = ":method"; value = "GET"; sensitive = false }
    ; { name = "field_not_indexed"; value = "foo"; sensitive = false }
    ; { name = "yet_another_field_not_indexed"
      ; value = "baz"
      ; sensitive = false
      }
    ]
  in
  let encoder = Encoder.create 60 in
  let encoded_headers = encode_headers encoder hs in
  Alcotest.(check bool)
    "Encodes to non-zero hex"
    true
    (String.length encoded_headers > 0);
  let decoder = Decoder.create 60 in
  let decoded_headers = decode_headers decoder 60 encoded_headers in
  List.iter2
    (fun h1 h2 ->
      Alcotest.(check header_testable "Decoded headers are roundtripped" h1 h2))
    hs
    decoded_headers

let () =
  let fixtures_dir = "hpack-test-case" in
  let raw_data_dir = Filename.concat fixtures_dir "raw-data" in
  let raw_data =
    raw_data_dir
    |> files_in_dir
    |> List.map (fun file -> file, Filename.concat raw_data_dir file)
    |> List.sort (fun (file, _) (file2, _) -> compare file file2)
  in
  (try Unix.mkdir (Filename.concat fixtures_dir "ocaml-hpack") 0o755 with
  | Unix.Unix_error (Unix.EEXIST, _, _) ->
    ());
  encode_raw_data fixtures_dir raw_data;
  (* Now, test decoding what we just encoded + roundtripping *)
  let fixtures = read_fixtures fixtures_dir in
  let suites = gen_suites fixtures in
  Alcotest.run
    "HPACK"
    (( "Handcrafted HPACK tests"
     , [ ( "Evictions from the dynamic table with 0 capacity"
         , `Quick
         , test_evicting_table_size_0 )
       ; ( "Evictions from the dynamic table with 0 capacity (followup test)"
         , `Quick
         , test_evicting_table_size_0_followup )
       ] )
    :: suites)
