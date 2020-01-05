
let
  sources = import ./nix/sources.nix {};
  inherit (sources) pkgs ocamlPackages;
  inherit (pkgs) stdenv lib;
  h2Pkgs = pkgs.recurseIntoAttrs (import ./nix { inherit sources; doCheck = false; });
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;

in
  (pkgs.mkShell {
    inputsFrom = lib.attrValues h2Drvs;
    buildInputs = with ocamlPackages; [ merlin pkgs.ocamlformat ];
  }).overrideAttrs (o : {
    propagatedBuildInputs = lib.filter
      (drv: drv.pname == null || !(lib.any (name: name == drv.pname) (lib.attrNames h2Drvs)))
      o.propagatedBuildInputs;
  })


