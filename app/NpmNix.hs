{-# LANGUAGE OverloadedStrings #-}

module NpmNix where

import Common
import qualified Console.Display as Display
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import NeatInterpolation (trimming)
import qualified Shelly
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

nodeTemplateDir :: FilePath -> FilePath
nodeTemplateDir executablePath = takeDirectory executablePath </> "../templates/node"

newtype NpmInitArgs = NpmInitArgs Text.Text deriving (Eq, Show)

init :: NpmInitArgs -> IO ()
init (NpmInitArgs _projectName) = do
  templateDir <- getTemplateDir "node"
  let copyNodeTemplate :: FilePath -> [(Text.Text, Text.Text)] -> IO ()
      copyNodeTemplate fileName replaceList =
        copyTemplate
          (templateDir </> fileName)
          (Text.unpack _projectName </> fileName)
          replaceList
  terminal <- Display.displayInit
  let step_ = step terminal

  step_ "Initializing directory and git..."
  Shelly.shelly $
    Shelly.silently $ do
      -- create the project dir
      -- initialize .git inside the project dir
      -- current dir=projectName
      mkDir _projectName
      mkGit

  -- currentDir=cwd
  step_ "Copying templates..."
  copyNodeTemplate
    "package.json"
    [ ("$packageName", _projectName),
      ("$author", "Dmitriy Skurikhin")
    ]
  copyNodeTemplate "flake.nix" [("$packageName", _projectName)]
  copyNodeTemplate ".gitignore" []
  copyNodeTemplate "tsconfig.json" []
  copyNodeTemplate "jest.config.js" []
  copyNodeTemplate ".eslintrc" []

  -- initialize flakes and commit inside the project dir
  -- current dir=projectName
  step_ "Initializing nix..."
  Shelly.shelly $
    Shelly.silently $ do
      Shelly.cd $ Text.unpack _projectName
      gitAdd
      initFlakes
      initCommit

  step_ "Finished"
