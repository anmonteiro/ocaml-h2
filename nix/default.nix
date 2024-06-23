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

    # These two don't have tests
    h2-lwt = buildDunePackage {
      pname = "h2-lwt";
      inherit version;
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "lwt" "h2-lwt.opam" ];
      };

      doCheck = false;
      propagatedBuildInputs = with ocamlPackages;
        [ lwt gluten-lwt ] ++ [ h2Pkgs.h2 ];
    };

    h2-lwt-unix = buildDunePackage {
      pname = "h2-lwt-unix";
      inherit version;
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "lwt-unix" "h2-lwt-unix.opam" ];
      };
      doCheck = false;
      propagatedBuildInputs = with ocamlPackages; [
        gluten-lwt-unix
        faraday-lwt-unix
        lwt_ssl
      ] ++ [ h2Pkgs.h2-lwt ];
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

    h2-mirage = buildDunePackage {
      pname = "h2-mirage";
      inherit version;
      src = with nix-filter; filter {
        root = ./..;
        include = [ "dune-project" "mirage" "h2-mirage.opam" ];
      };

      doCheck = false;
      propagatedBuildInputs = with ocamlPackages; [
        conduit-mirage
        gluten-mirage
      ] ++ [ h2Pkgs.h2-lwt ];
    };

  };
in

h2Pkgs // (if lib.versionOlder "5.0" ocaml.version then {
  h2-eio = buildDunePackage {
    pname = "h2-eio";
    inherit version;
    src = with nix-filter; filter {
      root = ./..;
      include = [ "dune-project" "eio" "h2-eio.opam" ];
    };

    propagatedBuildInputs = with ocamlPackages; [ gluten-eio ] ++ [ h2Pkgs.h2 ];
  };

} else { })
