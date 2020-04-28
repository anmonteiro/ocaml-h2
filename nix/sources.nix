{ ocamlVersion ? "4_10" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/ce000d4.tar.gz;
    sha256 = "0wrqm90cd66rqgpsm1xymjxb5xbjwmaiyw7dx7cx35xadpzil65b";
  };

in

  import "${overlays}/sources.nix" {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});
      })
    ];
  }
