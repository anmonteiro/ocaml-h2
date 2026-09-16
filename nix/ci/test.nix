{ ocamlVersion }:

let
  lock = builtins.fromJSON (builtins.readFile ./../../flake.lock);
  src = fetchGit {
    url = with lock.nodes.nixpkgs.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nixpkgs.locked) rev;
    allRefs = true;
  };

  nix-filter-src = fetchGit {
    url = with lock.nodes.nix-filter.locked; "https://github.com/${owner}/${repo}";
    inherit (lock.nodes.nix-filter.locked) rev;
    # inherit (lock.nodes.nixpkgs.original) ref;
    allRefs = true;
  };
  nix-filter = import "${nix-filter-src}";

  pkgs = import "${src}" {
    extraOverlays = [
      (self: super: {
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}";
      })
    ];
  };

  inherit (pkgs)
    lib
    stdenv
    fetchTarball
    ocamlPackages
    ;

  h2Pkgs = pkgs.callPackage ./.. { inherit nix-filter; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
  srcs = lib.mapAttrsToList (_: v: v.src) h2Drvs ++ [
    (
      with nix-filter;
      filter {
        root = ../..;
        include = [
          ".ocamlformat"
          ".ocamlformat-ignore"
        ];
      }
    )
  ];
in

stdenv.mkDerivation {
  name = "h2-tests";
  inherit srcs;
  sourceRoot = "./h2-tests";
  unpackPhase = ''
    shopt -s dotglob

    for src in $srcs; do
      cp -a $src "./testdir-$(basename $src)"
    done

    mkdir h2-tests

    chmod u+w -R testdir-*
    mv --backup=numbered testdir-*/* h2-tests
  '';
  dontBuild = true;
  installPhase = ''
    touch $out
  '';
  buildInputs =
    (lib.attrValues h2Drvs)
    ++ (with ocamlPackages; [
      ocaml
      dune
      findlib
      ocamlformat
    ]);
  checkInputs = with ocamlPackages; [
    alcotest
    hex
    yojson
  ];
  doCheck = true;
  checkPhase = ''
    # Check code is formatted with OCamlformat
    dune build --root=. @fmt

    # Build the examples
    dune build --display=short @install
  '';
}
