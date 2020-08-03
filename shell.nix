{ release-mode ? false }:

let
  pkgs = import ./nix/sources.nix {};
  inherit (pkgs) stdenv lib;
  h2Pkgs = pkgs.recurseIntoAttrs (import ./nix { inherit pkgs; doCheck = false; });
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;

in
  with pkgs;
  (mkShell {
    inputsFrom = lib.attrValues h2Drvs;
    buildInputs =
      (if release-mode then [
        cacert
        curl
        ocamlPackages.dune-release
        git
        opam
      ] else [])
      ++ (with ocamlPackages; [ merlin ocamlformat utop h2spec ]);
  }).overrideAttrs (o : {
    propagatedBuildInputs = lib.filter
      (drv: drv.pname == null || !(lib.any (name: name == drv.pname) (lib.attrNames h2Drvs)))
      o.propagatedBuildInputs;
  })


