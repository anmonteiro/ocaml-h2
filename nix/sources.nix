
{ pkgs ? import <nixpkgs> {}, ocamlVersion ? "4_09" }:

let
  gitignoreSrc = pkgs.fetchFromGitHub {
    owner = "hercules-ci";
    repo = "gitignore";
    rev = "7415c4f";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  };

  overlays = builtins.fetchTarball {
    url = https://github.com/anmonteiro/nix-overlays/archive/d2d883d84.tar.gz;
    sha256 = "1raq4wnv8h0v23kar79hra6xffkfl281zw4vzvyfr1fwgwwk7bx3";
  };

  inherit (import gitignoreSrc { inherit (pkgs) lib; }) gitignoreSource;

  ocamlPackages = pkgs.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope'
    (pkgs.callPackage "${overlays}/ocaml" { });

in
  { inherit pkgs ocamlPackages gitignoreSource; }
