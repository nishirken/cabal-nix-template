{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module NpmNix where

import qualified Data.Text as Text
import qualified Shelly
import NeatInterpolation (trimming)
import Common
import Data.Maybe (fromMaybe)

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


npmInit :: Shelly.Sh ()
npmInit = do
  let cmd = "nix-shell -p pkgs.nodePackages.npm --command \"npm init -y\""
  Shelly.bash_ cmd [] 

newtype NpmInitArgs = NpmInitArgs (Maybe Text.Text) deriving (Eq, Show)

init :: NpmInitArgs -> IO ()
init (NpmInitArgs _projectName)= do
  let projectName = fromMaybe "default-npm-project" _projectName
  Shelly.shelly $ do
    mkDir projectName
    mkFlakeFile $ flakeTemplate projectName
    mkGit gitignoreTemplate
    npmInit
    initFlakes
    initCommit

