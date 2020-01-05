{ pkgs ? import <nixpkgs> {}, ocamlVersion }:

let
  inherit (pkgs) lib stdenv fetchTarball;
  overlays = builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;
  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" { });

  h2Pkgs = import ./.. { inherit pkgs ocamlVersion; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
  h2spec = stdenv.mkDerivation {
    name = "h2spec";
    src = builtins.fetchurl {
      url = https://github.com/summerwind/h2spec/releases/download/v2.4.0/h2spec_linux_amd64.tar.gz;
      sha256 = "0566d35q5lgql07v7a21yrvgj8w2qymqxd1c3fqrvmria8g6iyjz";
    };
    phases = ["unpackPhase" "installPhase" "fixupPhase"];
    nativeBuildInputs = [ pkgs.autoPatchelfHook ];
    unpackPhase = ''
      mkdir h2spec-2.4.0
      tar -C h2spec-2.4.0 -xzf $src
    '';
    installPhase = ''
      mkdir -p $out/bin
      cp h2spec-2.4.0/* $out/bin
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
      nohup dune exec spec/lwt_h2spec_string.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      nohup dune exec spec/lwt_h2spec_bigstring.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      nohup dune exec spec/lwt_h2spec_streaming.exe &
      sleep 2
      ${h2spec}/bin/h2spec --strict -p 8080 --timeout 3
      kill $(${pkgs.lsof}/bin/lsof -i tcp:8080 | awk '{print $2}' | grep -v PID)

      # Check code is formatted with OCamlformat
      dune build @fmt
    '';
  }
