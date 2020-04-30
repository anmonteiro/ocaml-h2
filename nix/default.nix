{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_10"
, doCheck ? true }:

let
  inherit (pkgs) lib stdenv ocamlPackages;
in

  with ocamlPackages;

  # TODO: h2-async
  let
    buildH2 = args: buildDunePackage ({
      version = "0.6.0-dev";
      doCheck = doCheck;
      src = lib.gitignoreSource ./..;
    } // args);

    h2Packages = rec {
      hpack = buildH2 {
        pname = "hpack";
        buildInputs = [ alcotest hex yojson ];
        propagatedBuildInputs = [ angstrom faraday ];
        checkPhase = ''
          dune build @slowtests -p hpack --no-buffer --force
        '';
      };

      h2 = buildH2 {
        pname = "h2";
        buildInputs = [ alcotest hex yojson ];
        propagatedBuildInputs = [
          angstrom
          faraday
          base64
          psq
          hpack
          httpaf
        ];
      };

      # These two don't have tests
      h2-lwt = buildH2 {
        pname = "h2-lwt";
        doCheck = false;
        propagatedBuildInputs = [ h2 lwt4 gluten-lwt ];
      };

      h2-lwt-unix = buildH2 {
        pname = "h2-lwt-unix";
        doCheck = false;
        propagatedBuildInputs = [
          h2-lwt
          gluten-lwt-unix
          faraday-lwt-unix
          lwt_ssl
        ];
      };
    };
  in
  h2Packages // (if (lib.versionOlder "4.08" ocaml.version) then {
    h2-mirage = buildH2 {
      pname = "h2-mirage";
      doCheck = false;
      propagatedBuildInputs = [
        conduit-mirage
        h2-lwt
        gluten-mirage
      ];
    };
    } else {})
