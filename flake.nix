{
  description = "H2 Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays";
  # inputs.nixpkgs.url = "/home/anmonteiro/projects/nix-overlays";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          h2spec = super.stdenv.mkDerivation rec {
            name = "h2spec";
            version = "2.6.0";
            src = builtins.fetchurl (if super.stdenv.isDarwin then {
              url = "https://github.com/summerwind/h2spec/releases/download/v${version}/h2spec_darwin_amd64.tar.gz";
              sha256 = "008ngmlmj4slm13a2qvdyvfd6f3lxga2n0k300q3cpkg1bwvj74q";
            } else {
              url = "https://github.com/summerwind/h2spec/releases/download/v${version}/h2spec_linux_amd64.tar.gz";
              sha256 = "064k4yg818hd8pwh8xcf1iapw0k6ndsg1nsjwx0as09ff3gf0zhm";
            });
            phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
            nativeBuildInputs = (if super.stdenv.isDarwin then [ ] else [ self.autoPatchelfHook ]);
            unpackPhase = ''
              mkdir h2spec-${version}
              tar -C h2spec-${version} -xzf $src
            '';
            installPhase = ''
              mkdir -p $out/bin
              cp h2spec-${version}/* $out/bin
              chmod +x $out/bin/h2spec
            '';
          };

        });
      in
      rec {
        packages = pkgs.callPackage ./nix { inherit pkgs; };
        defaultPackage = packages.h2;
        devShell = import ./shell.nix { inherit pkgs; };
      });
}
