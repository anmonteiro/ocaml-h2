{ packages
, mkShell
, stdenv
, lib
, cacert
, curl
, ocamlPackages
, git
, release-mode ? false
}:

let
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) packages;

in


(mkShell {
  inputsFrom = lib.attrValues h2Drvs;
  buildInputs =
    (if release-mode then [
      cacert
      curl
      ocamlPackages.dune-release
      git
    ] else [ ])
    ++ (with ocamlPackages; [
      merlin
      ocamlformat
      utop
    ]);
}).overrideAttrs (o: {
  propagatedBuildInputs = lib.filter
    (drv: drv.pname == null || !(lib.any (name: name == drv.pname) (lib.attrNames h2Drvs)))
    o.propagatedBuildInputs;
})
