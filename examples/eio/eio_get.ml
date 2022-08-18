open H2
module Client = H2_eio.Client

let response_handler _response response_body =
  let rec read_response () =
    Body.Reader.schedule_read
      response_body
      ~on_eof:(fun () -> ())
      ~on_read:(fun bigstring ~off ~len ->
        Format.eprintf "heh nice %d@." len;
        let response_fragment = Bytes.create len in
        Bigstringaf.blit_to_bytes
          bigstring
          ~src_off:off
          response_fragment
          ~dst_off:0
          ~len;
        print_string (Bytes.to_string response_fragment);
        read_response ())
  in
  read_response ()

let error_handler _ = assert false

let () =
  let host = ref None in
  let port = ref 443 in
  Arg.parse
    [ "-p", Set_int port, " Port number (443 by default)" ]
    (fun host_argument -> host := Some host_argument)
    "lwt_get.exe [-p N] HOST";
  let host =
    match !host with
    | None -> failwith "No hostname provided"
    | Some host -> host
  in
  Eio_main.run (fun env ->
      Eio.Switch.run @@ fun sw ->
      let domain_mgr = Eio.Stdenv.domain_mgr env in
      let fd = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
      let addrs =
        Eio_unix.run_in_systhread (fun () ->
            Unix.getaddrinfo
              host
              (string_of_int !port)
              [ Unix.(AI_FAMILY PF_INET) ])
      in
      Eio_unix.run_in_systhread (fun () ->
          Unix.connect fd (List.hd addrs).ai_addr);
      let socket = Eio_unix.FD.as_socket ~sw ~close_unix:true fd in
      let request =
        Request.create
          `GET
          "/"
          ~scheme:"https"
          ~headers:Headers.(add_list empty [ ":authority", host ])
      in
      let connection =
        Client.SSL.create_connection_with_default
          ~domain_mgr
          ~error_handler
          socket
      in
      let request_body =
        Client.SSL.request connection request ~error_handler ~response_handler
      in
      Body.Writer.close request_body)
