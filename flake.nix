{
  description = "graphql";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    hls.url = "github:haskell/haskell-language-server";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, hls }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        packageName = "graphql";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghcid
            cabal-install
            implicit-hie
            haskell-language-server
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
