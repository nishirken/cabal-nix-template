{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module CabalNix where

import Common
import qualified Console.Display as Display
import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import NeatInterpolation (trimming)
import qualified Shelly
import System.FilePath ((</>))

cabalCmd :: Text.Text -> Text.Text
cabalCmd args = [trimming|nix-shell -p pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghc --command "cabal init $args"|]

initCabal :: Text.Text -> Text.Text -> Shelly.Sh ()
initCabal projectName args =
  let defaultArgs =
        [ "--minimal",
          "--quiet",
          "--package-name=" <> projectName,
          "--email=dmitrii.sk@gmail.com",
          "--author=Dmitrii\\ Skurihin"
        ]
      fullArgs = Text.unwords $ defaultArgs <> [args]
   in Shelly.bash_ (Text.unpack $ cabalCmd fullArgs) []

genHie :: Shelly.Sh ()
genHie = Shelly.bash_ "nix-shell -p pkgs.haskellPackages.implicit-hie --command \"gen-hie > hie.yaml\"" []

format :: Shelly.Sh ()
format = Shelly.bash_ "nix-shell -p pkgs.haskellPackages.ormolu --command \"./scripts/format.sh\"" []

data CabalInitParams = CabalInitParams
  { _projectName :: Text.Text,
    _cabalArgs :: Maybe Text.Text
  }
  deriving (Eq, Show)

init :: CabalInitParams -> IO ()
init CabalInitParams {..} = do
  let cabalArgs = fromMaybe "" _cabalArgs

  terminal <- Display.displayInit
  templateDir <- getTemplateDir "cabal"
  let copyTemplate_ :: FilePath -> [(Text.Text, Text.Text)] -> IO ()
      copyTemplate_ fileName replaceList =
        copyTemplate
          (templateDir </> fileName)
          (Text.unpack _projectName </> fileName)
          replaceList
      step_ = step terminal

  step_ "Initializing directory and git..."
  Shelly.shelly $
    Shelly.silently $ do
      -- create the project dir
      -- initialize .git inside the project dir
      -- current dir=projectName
      mkDir _projectName
      mkGit

  step_ "Initializing cabal..."
  Shelly.shelly $
    Shelly.silently $ do
      Shelly.cd $ Text.unpack _projectName
      initCabal _projectName cabalArgs

  step_ "Copying templates..."
  Shelly.shelly $
    Shelly.silently $ do
      Shelly.cp_r (templateDir </> "scripts") (Text.unpack _projectName </> "scripts")
      Shelly.bash_ ("chmod -R +x " <> (Text.unpack _projectName </> "scripts")) []
  copyTemplate_ "flake.nix" [("$packageName", _projectName)]
  copyTemplate_ ".gitignore" []

  step_ "Initializing flakes..."
  Shelly.shelly $
    Shelly.silently $ do
      Shelly.cd $ Text.unpack _projectName
      gitAdd
      initFlakes
      genHie
      format
      gitAdd
      initCommit

  step_ "Finished"
