{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified CabalNix
import qualified Console.Options as Cli
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified NpmNix

main :: IO ()
main = do
  Cli.defaultMain $ do
    projectName <- Cli.flagParam (Cli.FlagShort 'p' <> Cli.FlagLong "project-name") $ Cli.FlagRequired (Right . Text.pack)
    cabalArgs <- Cli.flagParam (Cli.FlagLong "cabal-args") $ Cli.FlagOptional "" (Right . Text.pack)

    Cli.command "cabal-nix" $
      Cli.action $ \toParam -> do
        let projectName' = toParam projectName
            cabalArgs' = toParam cabalArgs
        CabalNix.init $ CabalNix.CabalInitParams projectName' cabalArgs'

    Cli.command "npm-nix" $
      Cli.action $ \toParam -> do
        let projectName' = toParam projectName
        NpmNix.init $ NpmNix.NpmInitArgs projectName'
