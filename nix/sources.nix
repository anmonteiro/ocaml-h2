# nix-build -E \
#  'with import <nixpkgs> { overlays = [(import ./.)];}; pkgs.bs-platform'
{ ocamlVersion ? "4_09" }:

let
  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/98b6ee7.tar.gz;
    sha256 = "12vqdghlz2v7ln28jmrnxy210zv5x21g8kw2s0dvszfmz0p2inkh";
  };

  pkgs = import <nixpkgs> {
    overlays = [
      (import overlays)
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
            (super.callPackage "${overlays}/ocaml" {});
      })
    ];
  };

in
  pkgs
