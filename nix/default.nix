{ sources ? import ./sources.nix { inherit ocamlVersion; }, ocamlVersion ? "4_09", doCheck ? true }:

let
  inherit (sources) pkgs ocamlPackages gitignoreSource;
in
  pkgs.callPackage ./generic.nix {
    inherit ocamlPackages gitignoreSource doCheck;
  }

