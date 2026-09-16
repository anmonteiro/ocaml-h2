{ nix-filter, lib, stdenv, ocamlPackages, doCheck ? true }:

let
  inherit (ocamlPackages) buildDunePackage ocaml;
  version = "dev";
  h2Pkgs = rec {
    hpack = buildDunePackage {
      pname = "hpack";
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "hpack" "hpack.opam" ];
      };

      inherit version doCheck;
      checkInputs = with ocamlPackages; [ alcotest hex yojson ];
      propagatedBuildInputs = with ocamlPackages; [ angstrom faraday ];
      checkPhase = ''
        dune build @slowtests -p hpack --no-buffer --force
      '';
    };

    h2 = buildDunePackage {
      pname = "h2";
      inherit version doCheck;
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "lib" "lib_test" "h2.opam" ];
      };

      checkInputs = with ocamlPackages; [ alcotest hex yojson ];
      propagatedBuildInputs = with ocamlPackages; [
        angstrom
        faraday
        base64
        psq
        httpun
      ] ++ [ h2Pkgs.hpack ];
    };

    h2-async = buildDunePackage {
      pname = "h2-async";
      inherit version;
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "async" "h2-async.opam" ];
      };

      doCheck = false;
      propagatedBuildInputs = with ocamlPackages; [
        async
        gluten-async
        faraday-async
        async_ssl
      ] ++ [ h2Pkgs.h2 ];
    };

  };
in

h2Pkgs
