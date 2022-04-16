{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module CabalNix where

import qualified Shelly
import qualified Data.Text as Text
import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import NeatInterpolation (trimming)
import Common

flakeTemplate :: Text.Text -> Text.Text
flakeTemplate packageName =
  [trimming|{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      packageName = "$packageName";
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          $${packageName} =
            final.haskell-nix.cabalProject' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                tools = {
                  cabal = "3.6.2.0";
                  hlint = "latest";
                  haskell-language-server = "latest";
                  ormolu = "0.1.4.1";
                };
                buildInputs = [pkgs.haskellPackages.implicit-hie];
                withHoogle = true;
              };
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.$${packageName}.flake {};
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."$${packageName}:exe:$${packageName}";
    });
}|]

gitignoreTemplate :: Text.Text
gitignoreTemplate =
  Text.unlines
    [ "dist-newstyle",
      ".direnv"
    ]

cabalCmd :: Text.Text -> Text.Text
cabalCmd args = [trimming|nix-shell -p pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc --command "cabal init $args"|]

initCabal :: Text.Text -> Text.Text -> Shelly.Sh ()
initCabal projectName args =
  let defaultArgs =
        [ "--minimal"
        , "--quiet"
        , "--package-name=" <> projectName
        , "--email=dmitrii.sk@gmail.com"
        , "--author=Dmitrii\\ Skurihin"
        ]
      fullArgs = Text.unwords $ defaultArgs <> [args]
   in Shelly.bash_ (Text.unpack $ cabalCmd fullArgs) []

genHie :: Shelly.Sh ()
genHie = Shelly.bash_ "nix-shell -p pkgs.haskellPackages.implicit-hie --command \"gen-hie > hie.yaml\"" []

format :: Shelly.Sh ()
format = Shelly.bash_ "./scripts/format.sh" []

mkScripts :: Shelly.Sh ()
mkScripts = do
  let shebang = "#!/usr/bin/env bash"
  Shelly.mkdir "scripts"
  Shelly.touchfile "scripts/format.sh"
  Shelly.touchfile "scripts/lint.sh"
  Shelly.touchfile "scripts/check.sh"
  Shelly.writefile "scripts/format.sh" $ Text.unlines [shebang, "ormolu --mode inplace $(find . -name '*.hs')"]
  Shelly.writefile "scripts/lint.sh" $ Text.unlines [shebang, "hlint ."]
  Shelly.writefile "scripts/check.sh" $ Text.unlines [shebang, "./scripts/format.sh", "./scripts/lint.sh"]
  Shelly.bash_ "chmod +x scripts" []

data CabalInitParams = CabalInitParams
  { _projectName :: Maybe Text.Text
  , _cabalArgs :: Maybe Text.Text
  } deriving (Eq, Show)

init :: CabalInitParams -> IO ()
init CabalInitParams{..} = do
  Shelly.shelly $ do
    let
      projectName = fromMaybe "default-cabal-project" _projectName
      cabalArgs = fromMaybe "" _cabalArgs
    mkDir projectName
    initCabal projectName cabalArgs
    mkFlakeFile $ flakeTemplate projectName
    mkGit gitignoreTemplate
    mkScripts
    initFlakes
    genHie
    format
    initCommit

