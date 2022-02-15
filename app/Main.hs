{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Shelly
import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import NeatInterpolation (trimming)
import qualified Data.Text as Text

flakeTemplate :: Text.Text -> Text.Text
flakeTemplate packageName = [trimming|{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.$${system};
        haskellPackages = pkgs.haskellPackages;

        packageName = "$packageName";
      in {
        packages.$${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        defaultPackage = self.packages.$${system}.$${packageName};

        devShell = pkgs.mkShell {
          buildInputs = with haskellPackages; [
            ghcid
            cabal-install
            implicit-hie
            haskell-language-server
          ];
          inputsFrom = builtins.attrValues self.packages.$${system};
        };
      });
}|]

gitignoreTemplate :: Text.Text
gitignoreTemplate = Text.unlines
  [ "dist-newstyle"
  , ".direnv"
  ]

mkFlakeFile :: Text.Text -> Shelly.Sh ()
mkFlakeFile packageName = Shelly.writefile "flake.nix" $ flakeTemplate packageName

initFlakes :: Shelly.Sh ()
initFlakes = do
  Shelly.bash_ "git add ." []
  Shelly.bash_ "nix flake update" []

initCabal :: Maybe String -> Shelly.Sh ()
initCabal args = do
  let
    defaultArgs = ["-m", "--no-comments", "--overwrite"]
  Shelly.bash_ "cabal init" $ defaultArgs <> [Text.pack $ fromMaybe "" args]

genHie :: Shelly.Sh ()
genHie = Shelly.bash_ "gen-hie" []

mkGit :: Shelly.Sh ()
mkGit = do
  Shelly.bash_ "git init" []
  Shelly.writefile ".gitignore" gitignoreTemplate

main :: IO ()
main = do
    Cli.defaultMain $ do
        cabalArgs <- Cli.flagParam (Cli.FlagLong "cabal-args") $ Cli.FlagOptional "" Right
        Cli.action $ \toParam -> do
            Shelly.shelly $ do
                Shelly.mkdir "test"
                Shelly.cd "test"
                mkFlakeFile "test"
                initCabal $ toParam cabalArgs
                genHie
                mkGit
                initFlakes
                Shelly.bash_ "direnv allow" []
                Shelly.bash_ "git commit" ["-m 'initial'"]



