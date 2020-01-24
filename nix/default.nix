{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_09"
, doCheck ? true }:

pkgs.callPackage ./generic.nix {
  inherit doCheck;
}

