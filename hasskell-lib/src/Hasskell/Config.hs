module Hasskell.Config (Config (..)) where

import Data.Text

data Config = Config
  { token :: Text,
    baseUrl :: Text
  }
  deriving (Eq, Show)
