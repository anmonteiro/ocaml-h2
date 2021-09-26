{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages h2spec;

  h2Pkgs = import ./.. { inherit ocamlVersion; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
  srcs = lib.mapAttrsFlatten (n: v: v.src) h2Drvs ++ [
    (lib.filterGitSource {
      src = ../..;
      dirs = [ "spec" ];
      files = [ ".ocamlformat" ];
    })
  ];
in

stdenv.mkDerivation {
  name = "h2-conformance-tests";
  srcs = srcs;
  sourceRoot = "./h2-tests";
  unpackPhase = ''
    shopt -s dotglob

    for src in $srcs; do
      cp -a $src "./testdir-$(basename $src)"
    done

    mkdir h2-tests

    chmod u+w -R testdir-*
    mv -n testdir-*/* h2-tests
  '';
  dontBuild = true;
  installPhase = ''
    touch $out
  '';
  buildInputs =
    (lib.attrValues h2Drvs) ++
    (with ocamlPackages; [ ocaml dune findlib ]) ++
    (with pkgs; [ ocamlformat lsof h2spec ]);
  doCheck = true;
  checkPhase = ''
    # Check code is formatted with OCamlformat
    dune build --root=. @fmt

    dune build --root=. --display=short @spec/all
    dune exec --display=short spec/lwt_h2spec.exe &
    while [ -z "$(lsof -t -i tcp:8080)" ]; do
      sleep 1;
    done;

    h2spec --strict -p 8080 -P /string

    h2spec --strict -p 8080 -P /bigstring

    h2spec --strict -p 8080 --timeout 3 -P /streaming

    kill $(lsof -i tcp:8080 -t)
  '';
}
