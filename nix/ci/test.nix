{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages h2spec;

  h2Pkgs = import ./.. { inherit ocamlVersion; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
in
  stdenv.mkDerivation {
    name = "h2-conformance-tests";
    src = ./../..;
    dontBuild = true;
    installPhase = ''
      touch $out
    '';
    buildInputs = (lib.attrValues h2Drvs) ++ (with ocamlPackages; [ ocaml dune findlib pkgs.ocamlformat ]);
    doCheck = true;
    checkPhase = ''
      dune build spec/lwt_h2spec_string.exe
      nohup dune exec spec/lwt_h2spec_string.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      dune build spec/lwt_h2spec_bigstring.exe
      nohup dune exec spec/lwt_h2spec_bigstring.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      dune build spec/lwt_h2spec_streaming.exe
      nohup dune exec spec/lwt_h2spec_streaming.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080 --timeout 3
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      # Check code is formatted with OCamlformat
      dune build @fmt
    '';
  }
