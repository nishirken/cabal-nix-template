{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NpmNix where

import qualified Data.Text as Text
import NeatInterpolation (trimming)

flakeTemplate :: Text.Text -> Text.Text
flakeTemplate packageName =
  [trimming|{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.$${system};

        packageName = "$packageName";
      in {
        defaultPackage = self.packages.$${system}.$${packageName};

        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.nodejs-17_x
          ];
        };
      });
}|]

gitignoreTemplate :: Text.Text
gitignoreTemplate =
  Text.unlines
    [ ".direnv"
    ]

