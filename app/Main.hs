{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Console.Options as Cli
import qualified Shelly
import qualified Data.Text as Text
import qualified CabalNix
import qualified NpmNix
import Common
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  Cli.defaultMain $ do
    projectName <- Cli.flagParam (Cli.FlagShort 'p') $ Cli.FlagRequired (Right . Text.pack)
    cabalArgs <- Cli.flagParam (Cli.FlagLong "cabal-args") $ Cli.FlagOptional "" Right

    Cli.command "cabal-nix" $ Cli.action $ \toParam -> do
      let projectName' = fromMaybe "project" $ toParam projectName
      Shelly.shelly $ do
        mkDir projectName'
        CabalNix.initCabal projectName' $ toParam cabalArgs
        mkFlakeFile $ CabalNix.flakeTemplate projectName'
        mkGit CabalNix.gitignoreTemplate
        CabalNix.mkScripts
        initFlakes
        CabalNix.genHie
        CabalNix.format
        initCommit

    Cli.command "npm-nix" $ Cli.action $ \toParam -> do
      let projectName' = fromMaybe "project" $ toParam projectName
      Shelly.shelly $ do
        mkDir projectName'
        mkFlakeFile $ NpmNix.flakeTemplate projectName'
        mkGit NpmNix.gitignoreTemplate
        initFlakes
        initCommit
        
