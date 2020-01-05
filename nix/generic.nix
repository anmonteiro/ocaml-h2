{ lib, stdenv, ocamlPackages, gitignoreSource }:

with ocamlPackages;

let
  buildH2 = args: buildDunePackage ({
    version = "0.6.0-dev";
    doCheck = true;
    src = gitignoreSource ./..;
  } // args);

# TODO: h2-async, h2-mirage
in rec {
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
    propagatedBuildInputs = [ h2 lwt4 ];
  };

  h2-lwt-unix = buildH2 {
    pname = "h2-lwt-unix";
    doCheck = false;
    propagatedBuildInputs = [
      h2-lwt
      faraday-lwt-unix
      lwt_ssl
    ];
  };
}
