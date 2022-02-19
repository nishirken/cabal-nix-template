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
                  ormolu = "latest";
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

mkFlakeFile :: Text.Text -> Shelly.Sh ()
mkFlakeFile packageName = Shelly.writefile "flake.nix" $ flakeTemplate packageName

initFlakes :: Shelly.Sh ()
initFlakes = do
  Shelly.writefile ".envrc" "use flake"
  Shelly.bash_ "git add ." []
  Shelly.bash_ "nix flake update" []
  Shelly.bash_ "direnv allow" []

cabalCmd :: Text.Text -> Text.Text
cabalCmd args = [trimming|nix-shell -p pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc --command "cabal init $args"|]

initCabal :: Text.Text -> Maybe String -> Shelly.Sh ()
initCabal projectName args =
  let defaultArgs =
        [ "--minimal"
        , "--quiet"
        , "--package-name=" <> projectName
        , "--email=dmitrii.sk@gmail.com"
        , "--author=Dmitrii\\ Skurihin"
        ]
      fullArgs = Text.unwords $ defaultArgs <> [Text.pack $ fromMaybe "" args]
   in Shelly.bash_ (Text.unpack $ cabalCmd fullArgs) []

genHie :: Shelly.Sh ()
genHie = Shelly.bash_ "nix-shell -p pkgs.haskellPackages.implicit-hie --command gen-hie" []

mkGit :: Shelly.Sh ()
mkGit = do
  Shelly.bash_ "git init" ["-b master"]
  Shelly.writefile ".gitignore" gitignoreTemplate

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

mkDir :: Text.Text -> Shelly.Sh ()
mkDir dirName = do
  Shelly.mkdir $ Text.unpack dirName
  Shelly.cd $ Text.unpack dirName

main :: IO ()
main = do
  Cli.defaultMain $ do
    projectName <- Cli.flagParam (Cli.FlagShort 'p') $ Cli.FlagRequired (Right . Text.pack)
    cabalArgs <- Cli.flagParam (Cli.FlagLong "cabal-args") $ Cli.FlagOptional "" Right
    Cli.command "cabal-nix" $
      Cli.action $ \toParam -> do
        let projectName' = fromMaybe "project" $ toParam projectName
        Shelly.shelly $ do
          mkDir projectName'
          initCabal projectName' $ toParam cabalArgs
          mkFlakeFile projectName'
          mkGit
          mkScripts
          initFlakes
          genHie
          Shelly.bash_ "git commit" ["-a", "-m 'initial'"]
