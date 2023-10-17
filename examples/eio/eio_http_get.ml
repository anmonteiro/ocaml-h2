open H2
module Client = H2_eio.Client

let _response_handler ~on_eof response response_body =
  Format.eprintf "Response: %a@." Response.pp_hum response;

  let rec read_response () =
    Body.Reader.schedule_read
      response_body
      ~on_eof
      ~on_read:(fun bigstring ~off ~len ->
        let response_fragment = Bytes.create len in
        Bigstringaf.blit_to_bytes
          bigstring
          ~src_off:off
          response_fragment
          ~dst_off:0
          ~len;
        print_endline (Bytes.to_string response_fragment);
        read_response ())
  in
  read_response ()

let make_error_handler u err =
  (match err with
  | `Exn exn -> Format.eprintf "wut %S@." (Printexc.to_string exn)
  | `Invalid_response_body_length res ->
    Format.eprintf "invalid res: %a@." Response.pp_hum res
  | `Malformed_response str -> Format.eprintf "malformed %S@." str
  | `Protocol_error (err, s) ->
    Format.eprintf "pwut %a %S@." H2.Error_code.pp_hum err s);
  Eio.Promise.resolve u ()

let () =
  let host = ref None in
  let port = ref 8080 in
  Arg.parse
    [ "-p", Set_int port, " Port number (8080 by default)" ]
    (fun host_argument -> host := Some host_argument)
    "eio_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eio_main.run (fun env ->
    let network = Eio.Stdenv.net env in
    Eio.Switch.run (fun sw ->
      let addrs =
        let addrs =
          Eio_unix.run_in_systhread (fun () ->
            Unix.getaddrinfo
              host
              (string_of_int !port)
              [ Unix.(AI_FAMILY PF_INET) ])
        in
        List.filter_map
          (fun (addr : Unix.addr_info) ->
             match addr.ai_addr with
             | Unix.ADDR_UNIX _ -> None
             | ADDR_INET (addr, port) -> Some (addr, port))
          addrs
      in
      let addr =
        let inet, port = List.hd addrs in
        `Tcp (Eio_unix.Net.Ipaddr.of_unix inet, port)
      in
      let socket = Eio.Net.connect ~sw network addr in

      let request =
        Request.create
          `GET
          "/"
          ~scheme:"http"
          ~headers:
            Headers.(
              add_list
                empty
                [ "user-agent", "carl/0.0.0-experimental"; ":authority", host ])
      in

      let shut_p, shut_u = Eio.Promise.create () in
      let error_handler = make_error_handler shut_u in
      let connection = Client.create_connection ~sw ~error_handler socket in
      let response_handler =
        _response_handler ~on_eof:(fun () ->
          Format.eprintf "eof@.";
          if not (Eio.Promise.is_resolved shut_p)
          then Eio.Promise.resolve shut_u ())
      in
      let { H2.Client_connection.request_body; rst_stream = _ } =
        Client.request
          connection
          request
          ~error_handler
          ~response_handler
          ~flush_headers_immediately:true
      in
      Body.Writer.close request_body;
      Eio.Promise.await shut_p;
      Format.eprintf "Done@.";
      let ps = ref [] in
      for _i = 0 to 400 do
        let shut_p, shut_u = Eio.Promise.create () in
        let error_handler = make_error_handler shut_u in
        let response_handler =
          _response_handler ~on_eof:(fun () ->
            Format.eprintf "eof@.";
            if not (Eio.Promise.is_resolved shut_p)
            then Eio.Promise.resolve shut_u ())
        in

        let { H2.Client_connection.request_body; rst_stream } =
          Client.request
            connection
            request
            ~error_handler
            ~response_handler
            ~flush_headers_immediately:true
        in
        Body.Writer.close request_body;
        rst_stream ~code:H2.Error_code.EnhanceYourCalm;
        ps := shut_p :: !ps
      done;
      Eio.Fiber.all (List.map (fun p () -> Eio.Promise.await p) !ps);
      Eio.Promise.await (Client.shutdown connection)))
