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
          <*> option
            (eitherReader parseColor)
            ( long "color"
                <> metavar "MODE"
                <> help "Color output: auto | always | never"
                <> value ColorAuto
                <> showDefault
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
          { optionsHassToken = (optionsHassToken options <|> envApiToken)
          }

  logOptionsVerbose <- logOptionsHandle stderr (optionsVerbose options)
  let logOptionsColor = case optionsColor options of
          ColorAuto -> logOptionsVerbose
          ColorAlways -> setLogUseColor True logOptionsVerbose
          ColorNever -> setLogUseColor False logOptionsVerbose
  pc <- mkDefaultProcessContext
  withLogFunc logOptionsColor $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = environmentAugmentedOptions
            }
     in runRIO app run

parseColor :: String -> Either String ColorMode
parseColor s = case s of
  "auto" -> Right ColorAuto
  "always" -> Right ColorAlways
  "never" -> Right ColorNever
  _ -> Left "Expected 'auto', 'always', or 'never'"
