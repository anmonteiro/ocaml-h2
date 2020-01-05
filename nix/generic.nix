{ lib, stdenv, ocamlPackages }:

with ocamlPackages;

let
  buildH2 = args: buildDune2Package ({
    version = "0.6.0-dev";
    src = lib.cleanSource ./..;
  } // args);

# TODO: h2-async, h2-mirage
in rec {
  hpack = buildH2 {
    pname = "hpack";
    propagatedBuildInputs = [ angstrom faraday ];
  };

  h2 = buildH2 {
    pname = "h2";
    propagatedBuildInputs = [
      angstrom
      faraday
      base64
      psq
      hpack
      httpaf
    ];
  };

  h2-lwt = buildH2 {
    pname = "h2-lwt";
    propagatedBuildInputs = [ h2 lwt4 ];
  };

  h2-lwt-unix = buildH2 {
    pname = "h2-lwt-unix";
    propagatedBuildInputs = [
      h2-lwt
      faraday-lwt-unix
      lwt_ssl
    ];
  };
}
