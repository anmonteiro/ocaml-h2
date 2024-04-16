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
        ocamlPackages = super.ocaml-ng."ocamlPackages_${ocamlVersion}".overrideScope (oself: osuper: {
          gluten-lwt-unix = osuper.gluten-lwt-unix.overrideAttrs (o: {
            propagatedBuildInputs = o.propagatedBuildInputs ++ [ oself.tls-lwt ];
          });
        });
      })
    ];
  };


  inherit (pkgs) lib stdenv fetchTarball ocamlPackages h2spec;

  h2Pkgs = import ./.. { inherit pkgs nix-filter; };
  h2Drvs = lib.filterAttrs (_: value: lib.isDerivation value) h2Pkgs;
  srcs = lib.mapAttrsFlatten (n: v: v.src) h2Drvs ++ [
    (with nix-filter; filter {
      root = ../..;
      include = [ "spec" ".ocamlformat" ];
    })
  ];
in

stdenv.mkDerivation {
  name = "h2-conformance-tests";
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
    (lib.attrValues h2Drvs) ++
    (with ocamlPackages; [ ocaml dune findlib ocamlformat httpaf-lwt-unix ]) ++
    (with pkgs; [ lsof h2spec ]);
  checkInputs = with ocamlPackages; [ alcotest hex yojson ];
  doCheck = true;
  checkPhase = ''
    # Check code is formatted with OCamlformat
    dune build --root=. @fmt

    # Build the examples
    dune build @all --display=short

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
