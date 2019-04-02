open H2

module Headers_tests = struct
  let test_headers_roundtrip_ordering () =
    let headers_list = [ "a", "1"; "b", "2"; "c", "3" ] in
    Alcotest.(check (list (pair string string)))
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

  let suite = [ "roundtripping", `Quick, test_headers_roundtrip_ordering ]
end

let () = Alcotest.run "ocaml-h2 unit tests" [ "headers", Headers_tests.suite ]
