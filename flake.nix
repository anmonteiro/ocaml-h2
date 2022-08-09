{
  description = "H2 Nix Flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:anmonteiro/nix-overlays?rev=5d3c6e7c37bf9487a88cc5466dd632282cea928c";
  inputs.nixpkgs.inputs.flake-utils.follows = "flake-utils";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages."${system}".extend (self: super: {
          h2spec = super.callPackage ./nix/h2spec.nix { };

        });
      in
      rec {
        packages = pkgs.callPackage ./nix { };
        defaultPackage = packages.h2;
        devShell = pkgs.callPackage ./shell.nix { };
      });
}
