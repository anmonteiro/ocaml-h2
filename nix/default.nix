{ pkgs ? import <nixpkgs> {}, ocamlVersion ? "4_09" }:

let
  overlays = builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;

  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" { });
in
  pkgs.callPackage ./generic.nix {
    inherit ocamlPackages;
  }

