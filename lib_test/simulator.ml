open Http2af__
(* open Http2af__.Httpaf_private *)

let debug msg =
  if true then Printf.eprintf "%s\n%!" msg

let to_frame encoder { Request.headers; meth; target; _ } =
  let faraday = Faraday.create 0x1000 in
  let headers = Headers.add_list headers
    [ ":scheme", "http"
    ; ":method", Httpaf.Method.to_string meth
    ; ":path", target
    ]
  in
  List.iter (Hpack.Encoder.encode_header encoder faraday) headers;
  Frame.Headers (None, Faraday.serialize_to_bigstring faraday)

let write_request t encoder req =
  let frame_payload = to_frame encoder req in
  let frame_header =
    { Frame
    . flags = Flags.(set_end_header default_flags)
    ; stream_id = Int32.one
    ; payload_length = 0
    ; frame_type = Headers
    }
  in
  Serialize.write_frame t { frame_header; frame_payload }

let request_to_string conn r =
  let f = Faraday.create 0x1000 in
  Serialize.write_connection_preface f;
  write_request f conn.Server_connection.encoder r;
  Faraday.serialize_to_string f

let response_to_string conn r =
  let open Serialize in
  let writer = Writer.create () in
  let frame_info =
    Writer.make_frame_info
      ~flags:Frame.Flags.(set_end_header default_flags)
      ~stream_id:1l
  in
  Serialize.Writer.write_headers writer conn.Server_connection.encoder frame_info r;
  Faraday.serialize_to_string writer.encoder

let body_to_strings = function
  | `Empty       -> []
  | `Data   xs  -> xs
;;

let case_to_strings conn = function
  | `Request  r, body -> [request_to_string conn r] @ (body_to_strings body)
  | `Response r, body -> [response_to_string conn r] @ (body_to_strings body)

let response_stream_to_body conn (`Response response, body) =
  let response = response_to_string conn response in
  match body with
  | `Empty  -> response
  | `Data xs -> String.concat "" (response :: xs)

let iovec_to_string { IOVec.buffer; off; len } =
  Bigstringaf.substring ~off ~len buffer

let bigstring_append_string bs s =
 let bs_len = Bigstringaf.length bs in
  let s_len  = String.length s in
  let bs' = Bigstringaf.create (bs_len + s_len) in
  Bigstringaf.blit             bs ~src_off:0 bs' ~dst_off:0      ~len:bs_len;
  Bigstringaf.blit_from_string s  ~src_off:0 bs' ~dst_off:bs_len ~len:s_len;
  bs'

let bigstring_empty = Bigstringaf.empty

let test_server ~input ~output ~handler () =
  let conn   = Server_connection.create handler in
  let reads  = List.(concat (map (case_to_strings conn) input)) in
  (* let `Hex hex = (Hex.of_string (String.concat "" reads)) in
  Printf.eprintf "READS '%s'\n%!" hex ; *)
  let writes = List.(concat (map (case_to_strings conn) output)) in
  let iwait, owait = ref false, ref false in
  let rec loop conn input reads =
    if !iwait && !owait then
      assert false (* deadlock, at lest for test handlers. *);
    if Server_connection.is_closed conn
    then begin
      debug "state: closed";
      []
    end else begin
      let input', reads' = iloop conn input reads in
      let output         = oloop conn in
      output @ loop conn input' reads'
    end
  and iloop conn input reads =
    if !iwait
    then begin debug " iloop: wait"; input, reads end
    else
      match Server_connection.next_read_operation conn, reads with
      | `Read, read::reads' ->
        debug " server iloop: read";
        let input     = bigstring_append_string input read in
        let input_len = Bigstringaf.length input in
        let result    = Server_connection.read conn input ~off:0 ~len:input_len in
        if result = input_len
        then bigstring_empty, reads'
        else Bigstringaf.sub ~off:result  ~len:(input_len - result) input, reads'
      | `Read, [] ->
        debug " server iloop: eof";
        let input_len = Bigstringaf.length input in
        ignore (Server_connection.read_eof conn input ~off:0 ~len:input_len : int);
        bigstring_empty, []
      | _          , [] ->
        debug " server iloop: eof";
        let input_len = Bigstringaf.length input in
        ignore (Server_connection.read_eof conn input ~off:0 ~len:input_len : int);
        bigstring_empty, []
      | `Close    , _     ->
        debug " server iloop: close(ok)"; input, []
      | `Yield , _  ->
        debug " server iloop: yield";
        iwait := true;
        Server_connection.yield_reader conn (fun () -> debug " iloop: continue"; iwait := false);
        input, reads
  and oloop conn =
    if !owait
    then (begin debug " server oloop: wait"; [] end)
    else
      match Server_connection.next_write_operation conn with
      | `Close _ ->
        debug " server oloop: closed"; []
      | `Yield ->
        debug " server oloop: yield";
        owait := true;
        Server_connection.yield_writer conn (fun () -> debug " server oloop: continue"; owait := false);
        []
      | `Write iovecs ->
        debug " server oloop: write";
        let output = List.map iovec_to_string iovecs in
        Server_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
        (* HACK, remove *)
        Server_connection.shutdown conn;
        output
  in
  let test_output = loop conn bigstring_empty reads |> String.concat "" in
  let output      = String.concat "" writes in
  Printf.eprintf "wats diff: '%s' '%s'" (Hex.of_string output |> Hex.show) (Hex.of_string test_output |> Hex.show);
  Alcotest.(check string "response" output test_output)
;;

(* let test_client ~request ~request_body_writes ~response_stream () =
  let reads  = case_to_strings response_stream in
  let writes = case_to_strings (`Request request, `Fixed request_body_writes) in
  let test_input  = ref []    in
  let got_eof     = ref false in
  let error_handler _ = assert false in
  let response_handler response response_body =
    test_input := (response_to_string response) :: !test_input;
    let rec on_read bs ~off ~len =
      test_input := Bigstring.to_string bs ~off ~len :: !test_input;
      Body.schedule_read response_body ~on_read ~on_eof
    and on_eof () = got_eof := true in
    Body.schedule_read response_body ~on_read ~on_eof
  in
  let body, conn =
    Client_connection.request
      request
      ~error_handler
      ~response_handler
  in
  let rec loop conn request_body_writes input reads =
    if Client_connection.is_closed conn
    then []
    else begin
      let input', reads'               = iloop conn input reads in
      let output, request_body_writes' = oloop conn request_body_writes in
      output @ loop conn request_body_writes' input' reads'
    end
  and oloop conn request_body =
    let request_body' =
      match request_body with
      | []      -> Body.close_writer body; request_body
      | x :: xs -> Body.write_string body x; Body.flush body ignore; xs
    in
    match Client_connection.next_write_operation conn with
    | `Yield   ->
      (* This should only happen once to close the writer *)
      Client_connection.yield_writer conn ignore; [], request_body'
    | `Close _ ->
      debug " client oloop: closed"; [], request_body'
    | `Write iovecs ->
      debug " client oloop: write";
      let output = List.map iovec_to_string iovecs in
      Client_connection.report_write_result conn (`Ok (IOVec.lengthv iovecs));
      output, request_body'
  and iloop conn input reads =
    match Client_connection.next_read_operation conn, reads with
    | `Read, read::reads' ->
      debug " client iloop: read";
      let input     = bigstring_append_string input read in
      let input_len = Bigstring.length input in
      let result     = Client_connection.read conn input ~off:0 ~len:input_len in
      if result = input_len
      then bigstring_empty, reads'
      else Bigstring.sub ~off:result input, reads'
    | `Read, [] ->
      debug " client iloop: eof";
      let input_len = Bigstring.length input in
      ignore (Client_connection.read_eof conn input ~off:0 ~len:input_len : int);
      input, []
    | _          , [] ->
      debug " client iloop: eof";
      let input_len = Bigstring.length input in
      ignore (Client_connection.read_eof conn input ~off:0 ~len:input_len : int);
      input, []
    | `Close    , _     ->
      debug " client iloop: close(ok)";
      input, []
  in
  let test_output = loop conn request_body_writes bigstring_empty reads |> String.concat "" in
  let test_input  = List.rev !test_input |> String.concat "" in
  let input       = response_stream_to_body response_stream in
  let output      = String.concat "" writes in
  Alcotest.(check bool   "got eof"  true   !got_eof);
  Alcotest.(check string "request"  output test_output);
  Alcotest.(check string "response" input  test_input);
;; *)
