{ ocamlVersion ? "4_10" }:

let
  overlays =
    builtins.fetchTarball
      https://github.com/anmonteiro/nix-overlays/archive/70347e97.tar.gz;

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});

        h2spec = super.stdenv.mkDerivation rec {
          name = "h2spec";
          version = "2.5.0";
          src = builtins.fetchurl {
            url = "https://github.com/summerwind/h2spec/releases/download/v${version}/h2spec_linux_amd64.tar.gz";
            sha256 = "1za8r8fz57w68qmx4r9drd1wj44w7h6dnpcadp1j7sqn790i4l4g";
          };
          phases = ["unpackPhase" "installPhase" "fixupPhase"];
          nativeBuildInputs = [ self.autoPatchelfHook ];
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
      })
    ];
  }
