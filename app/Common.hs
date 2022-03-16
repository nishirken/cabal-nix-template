{-# LANGUAGE OverloadedStrings #-}

module Common where

import qualified Data.Text as Text
import qualified Shelly

initCommit :: Shelly.Sh ()
initCommit = Shelly.bash_ "git commit" ["-a", "-m 'initial'"]

mkDir :: Text.Text -> Shelly.Sh ()
mkDir dirName = do
  Shelly.mkdir $ Text.unpack dirName
  Shelly.cd $ Text.unpack dirName

mkGit :: Text.Text -> Shelly.Sh ()
mkGit gitignoreTemplate = do
  Shelly.bash_ "git init" ["-b master"]
  Shelly.writefile ".gitignore" gitignoreTemplate

mkFlakeFile :: Text.Text -> Shelly.Sh ()
mkFlakeFile template = Shelly.writefile "flake.nix" template

initFlakes :: Shelly.Sh ()
initFlakes = do
  Shelly.writefile ".envrc" "use flake"
  Shelly.bash_ "git add ." []
  Shelly.bash_ "nix flake update" []
  Shelly.bash_ "direnv allow" []

