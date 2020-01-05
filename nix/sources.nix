
{ pkgs ? import <nixpkgs> {}, ocamlVersion ? "4_09" }:

let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "7415c4f";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };

  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  overlays = builtins.fetchTarball https://github.com/anmonteiro/nix-overlays/archive/master.tar.gz;
  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" { });

in
  { inherit pkgs ocamlPackages gitignoreSource; }
