{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import NeatInterpolation (trimming)
import qualified Shelly

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
        haskellPackages = pkgs.haskellPackages;

        packageName = "$packageName";
      in {
        packages.$${packageName} =
          haskellPackages.callCabal2nix packageName self rec {};

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
gitignoreTemplate =
  Text.unlines
    [ "dist-newstyle",
      ".direnv"
    ]

mkFlakeFile :: Text.Text -> Shelly.Sh ()
mkFlakeFile packageName = Shelly.writefile "flake.nix" $ flakeTemplate packageName

initFlakes :: Shelly.Sh ()
initFlakes = do
  Shelly.writefile ".envrc" "use flake"
  Shelly.bash_ "git add ." []
  Shelly.bash_ "nix flake update" []
  Shelly.bash_ "direnv allow" []

initCabal :: Text.Text -> Maybe String -> Shelly.Sh ()
initCabal projectName args = do
  let defaultArgs =
        [ "--minimal",
          "--quiet",
          "--email=dmitrii.sk@gmail.com",
          "--author=Dmitrii\\ Skurihin"
        ]
  Shelly.bash_ "cabal init" $ defaultArgs <> [Text.pack $ fromMaybe "" args]

genHie :: Shelly.Sh ()
genHie = Shelly.bash_ "gen-hie" []

mkGit :: Shelly.Sh ()
mkGit = do
  Shelly.bash_ "git init" ["-b master"]
  Shelly.writefile ".gitignore" gitignoreTemplate

test :: Shelly.Sh ()
test = do
  Shelly.mkdir "/tmp/test"
  Shelly.cd "/tmp/test"

main :: IO ()
main = do
  Cli.defaultMain $ do
    projectName <- Cli.flagParam (Cli.FlagShort 'p') $ Cli.FlagRequired (Right . Text.pack)
    cabalArgs <- Cli.flagParam (Cli.FlagLong "cabal-args") $ Cli.FlagOptional "" Right
    Cli.command "cabal-nix" $
      Cli.action $ \toParam -> do
        let projectName' = fromMaybe "project" $ toParam projectName
        Shelly.shelly $ do
          mkFlakeFile projectName'
          mkGit
          initCabal projectName' $ toParam cabalArgs
          initFlakes
          genHie
          Shelly.bash_ "git commit" ["-a", "-m 'initial'"]
