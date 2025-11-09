{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import Paths_hasskell_cli qualified
import RIO.Process
import RIO.Text (pack)
import Run
import System.Environment (lookupEnv)

main :: IO ()
main = do
  envApiToken <- (fmap pack) <$> (lookupEnv "HASSKELL_HASS_API_TOKEN")
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_hasskell_cli.version)
      "hasskell - A Home Assistant automation framework in Haskell"
      ""
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
          <*> optional
            ( strOption
                ( long "hass-token"
                    <> metavar "TOKEN"
                    <> help "Home Assistant API token (or from environment variable HASSKELL_HASS_API_TOKEN)"
                )
            )
      )
      empty

  let environmentAugmentedOptions =
        options
          { hassToken = (hassToken options <|> envApiToken)
          }

  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = environmentAugmentedOptions
            }
     in runRIO app run
