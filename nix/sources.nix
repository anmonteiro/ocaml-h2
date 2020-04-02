{ ocamlVersion ? "4_10" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/79040d1.tar.gz;
    sha256 = "0a7sdxihp8nsggh3a535b0gb9mnjbwa48d4fj2rw4n18223la1wg";
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
