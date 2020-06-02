{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

  h2Pkgs = import ./.. { inherit ocamlVersion; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
  h2spec = stdenv.mkDerivation rec {
    name = "h2spec";
    version = "2.5.0";
    src = builtins.fetchurl {
      url = "https://github.com/summerwind/h2spec/releases/download/v${version}/h2spec_linux_amd64.tar.gz";
      sha256 = "1za8r8fz57w68qmx4r9drd1wj44w7h6dnpcadp1j7sqn790i4l4g";
    };
    phases = ["unpackPhase" "installPhase" "fixupPhase"];
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    unpackPhase = ''
      mkdir h2spec-${version}
      tar -C h2spec-${version} -xzf $src
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp h2spec-${version}/* $out/bin
      chmod +x $out/bin/h2spec
    '';
  };
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
