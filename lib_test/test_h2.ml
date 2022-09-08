open H2

module Headers_tests = struct
  let check = Alcotest.(check (list (pair string string)))

  let test_headers_roundtrip_ordering () =
    let headers_list = [ "a", "1"; "b", "2"; "c", "3" ] in
    check
      "to_list / of_list"
      Headers.(to_list (of_list headers_list))
      headers_list;
    Alcotest.(check (option string))
      "get / of_list"
      Headers.(get (of_list [ "k", "v1"; "k", "v2" ]) "k")
      (Some "v2");
    Alcotest.(check (option string))
      "get / of_rev_list"
      Headers.(get (of_rev_list [ "k", "v1"; "k", "v2" ]) "k")
      (Some "v1");
    let headers = Headers.(add_list empty headers_list) in
    Alcotest.(check (option string))
      "add / get"
      Headers.(get (add headers "foo" "bar") "foo")
      (Some "bar");
    let hs = Headers.(add (add empty "foo" "bar") "foo" "other") in
    Alcotest.(check (option string))
      "add / get"
      Headers.(get hs "foo")
      (Some "other")

  let test_remove () =
    check
      "remove leading element"
      [ "c", "d" ]
      (Headers.remove (Headers.of_list [ "a", "b"; "c", "d" ]) "a"
      |> Headers.to_list);
    check
      "remove trailing element"
      [ "c", "d" ]
      (Headers.remove (Headers.of_list [ "c", "d"; "a", "b" ]) "a"
      |> Headers.to_list);
    check
      "remove trailing element"
      [ "c", "d"; "e", "f" ]
      (Headers.remove (Headers.of_list [ "c", "d"; "e", "f"; "a", "b" ]) "a"
      |> Headers.to_list);
    check
      "remove trailing element"
      [ "c", "d"; "e", "f"; "g", "h" ]
      (Headers.remove
         (Headers.of_list [ "c", "d"; "e", "f"; "a", "b"; "g", "h" ])
         "a"
      |> Headers.to_list)

  let test_replace () =
    check
      "replace leading element"
      [ "a", "x"; "c", "d" ]
      (Headers.replace (Headers.of_list [ "a", "b"; "c", "d" ]) "a" "x"
      |> Headers.to_list);
    check
      "replace trailing element"
      [ "c", "d"; "a", "x" ]
      (Headers.replace (Headers.of_list [ "c", "d"; "a", "b" ]) "a" "x"
      |> Headers.to_list);
    check
      "replace trailing element"
      [ "c", "d"; "e", "f"; "a", "x" ]
      (Headers.replace
         (Headers.of_list [ "c", "d"; "e", "f"; "a", "b" ])
         "a"
         "x"
      |> Headers.to_list);
    check
      "replace trailing element"
      [ "c", "d"; "e", "f"; "a", "x"; "g", "h" ]
      (Headers.replace
         (Headers.of_list [ "c", "d"; "e", "f"; "a", "b"; "g", "h" ])
         "a"
         "x"
      |> Headers.to_list);
    check
      "replace middle element"
      [ "e", "f"; "c", "z"; "a", "b" ]
      (Headers.replace
         (Headers.of_list [ "e", "f"; "c", "d"; "a", "b" ])
         "c"
         "z"
      |> Headers.to_list)

  let suite =
    [ "roundtripping", `Quick, test_headers_roundtrip_ordering
    ; "test remove", `Quick, test_remove
    ; "test replace", `Quick, test_replace
    ]
end

module Body_length_tests = struct
  let test_request () =
    let content_length_request =
      Request.create
        ~headers:(Headers.of_list [ "content-length", "10" ])
        ~scheme:"https"
        `GET
        "/"
    in
    let invalid_content_length_request =
      Request.create
        ~headers:(Headers.of_list [ "content-length", "NaN" ])
        ~scheme:"https"
        `GET
        "/"
    in
    let no_content_length_request = Request.create ~scheme:"https" `GET "/" in
    (match Request.body_length content_length_request with
    | `Fixed 10L -> ()
    | _ -> Alcotest.fail "Expected `Fixed 10L");
    (match Request.body_length invalid_content_length_request with
    | `Error `Bad_request -> ()
    | _ -> Alcotest.fail "Expected `Error `Bad_request");
    match Request.body_length no_content_length_request with
    | `Unknown -> ()
    | _ -> Alcotest.fail "Expected `Unknown"

  let test_response () =
    let content_length_response =
      Response.create ~headers:(Headers.of_list [ "content-length", "10" ]) `OK
    in
    let invalid_content_length_response =
      Response.create ~headers:(Headers.of_list [ "content-length", "NaN" ]) `OK
    in
    let no_content_length_response = Response.create `OK in
    (match
       Response.body_length ~request_method:`GET content_length_response
     with
    | `Fixed 10L -> ()
    | _ -> Alcotest.fail "Expected `Fixed 10L");
    (match
       Response.body_length ~request_method:`HEAD content_length_response
     with
    | `Fixed 0L -> ()
    | _ -> Alcotest.fail "Expected `Fixed 0L");
    (match
       Response.body_length ~request_method:`GET invalid_content_length_response
     with
    | `Error `Bad_request -> ()
    | _ -> Alcotest.fail "Expected `Error `Bad_request");
    match
      Response.body_length ~request_method:`GET no_content_length_response
    with
    | `Unknown -> ()
    | _ -> Alcotest.fail "Expected `Unknown"

  let suite =
    [ "request", `Quick, test_request; "response", `Quick, test_response ]
end

let () =
  Alcotest.run
    "ocaml-h2 unit tests"
    [ "headers", Headers_tests.suite; "body lengths", Body_length_tests.suite ]
