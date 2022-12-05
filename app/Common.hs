{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Basement.Bounded as BB
import qualified Basement.Imports as BI
import qualified Console.Display as Display
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import qualified Shelly
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory, (</>))

gitAdd :: Shelly.Sh ()
gitAdd = Shelly.bash_ "git add ." []

initCommit :: Shelly.Sh ()
initCommit = do
  gitAdd
  Shelly.bash_ "git commit" ["-m 'initial'"]

mkDir :: Text.Text -> Shelly.Sh ()
mkDir dirName = do
  Shelly.mkdir $ Text.unpack dirName
  Shelly.cd $ Text.unpack dirName

mkGit :: Shelly.Sh ()
mkGit = do
  Shelly.bash_ "git init" ["-b master"]

mkFlakeFile :: Text.Text -> Shelly.Sh ()
mkFlakeFile = Shelly.writefile "flake.nix"

initFlakes :: Shelly.Sh ()
initFlakes = do
  Shelly.writefile ".envrc" "use flake"
  gitAdd
  Shelly.bash_ "nix flake update" []
  Shelly.bash_ "direnv allow" []

replaceAll :: Text.Text -> [(Text.Text, Text.Text)] -> Text.Text
replaceAll = foldl (\acc (xs, ys) -> Text.replace xs ys acc)

getTemplate :: FilePath -> [(Text.Text, Text.Text)] -> IO Text.Text
getTemplate templatePath replaceList = do
  content <- TIO.readFile templatePath
  pure $ replaceAll content replaceList

copyTemplate :: FilePath -> FilePath -> [(Text.Text, Text.Text)] -> IO ()
copyTemplate templatePath targetPath replaceList = do
  content <- getTemplate templatePath replaceList
  TIO.writeFile targetPath content

-- Display
whiteColor :: Display.ColorComponent
whiteColor = BB.zn64 7

step :: Display.TerminalDisplay -> BI.String -> IO ()
step td stepName = do
  Display.displayLn td whiteColor stepName

getTemplateDir :: FilePath -> IO FilePath
getTemplateDir dir = do
  executablePath <- getExecutablePath
  pure $ takeDirectory executablePath </> "../templates" </> dir
