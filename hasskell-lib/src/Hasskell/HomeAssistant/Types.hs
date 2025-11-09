{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Types used by Home Assistant.
module Hasskell.HomeAssistant.Types
  ( HASSAuthMessage (..),
    HASSAuthResponse (..),
    UnitSystem (..),
    HASSConfig (..),
    Service (..),
    ServiceDomain (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Char (toLower)
import Data.Map.Lazy qualified as M
import Data.Text (Text)
import Deriving.Aeson
import GHC.TypeLits
import Network.WebSockets qualified as WS

--------------------------------------------------------------------------------

data Decapitalize

instance StringModifier Decapitalize where
  getStringModifier = decapitalize
    where
      decapitalize :: String -> String
      decapitalize [] = []
      decapitalize (c : rest) = toLower c : rest

-- | The custom JSON encoding/decoding options for HA types.
--
-- HA does not like fields that are set to @null@ - they must be omitted.
type JSONOptions (prefix :: Symbol) =
  '[ OmitNothingFields,
     FieldLabelModifier '[StripPrefix prefix, CamelToSnake],
     TagSingleConstructors,
     SumTaggedObject "type" "",
     ConstructorTagModifier '[Decapitalize, StripPrefix prefix, CamelToSnake]
   ]

--------------------------------------------------------------------------------

-- | Initial authentication responses.
data HASSAuthResponse
  = ResponseAuthRequired {responseHaVersion :: Text}
  | ResponseAuthInvalid {responseMessage :: Text}
  | ResponseAuthOk {responseHaVersion :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "response") HASSAuthResponse

-- | Authentication message sent during initial handshake.
data HASSAuthMessage = MessageAuth {messageAccessToken :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "message") HASSAuthMessage

instance WS.WebSocketsData HASSAuthMessage where
  fromLazyByteString = undefined
  fromDataMessage = undefined
  toLazyByteString = encode

-- | Represents units that Home Assistant is configured to use.
data UnitSystem = MkUnitSystem
  { -- | Identifies the unit used for distances.
    unitSystemLength :: Text,
    -- | Identifies the unit used for mass.
    unitSystemMass :: Text,
    -- | Identifies the unit used for temperatures.
    unitSystemTemperature :: Text,
    -- | Identifies the unit used for volumes.
    unitSystemVolume :: Text
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "unitSystem") UnitSystem

-- | Represents Home Assistant configurations.
data HASSConfig = MkConfig
  { configComponents :: [Text],
    configConfigDir :: Text,
    configLocationName :: Text,
    configTimeZone :: Text,
    configElevation :: Int,
    configLatitude :: Double,
    configLongitude :: Double,
    -- | The units that Home Assistant is configured to use.
    configUnitSystem :: UnitSystem,
    configVersion :: Text,
    configWhitelistExternalDirs :: [Text]
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "config") HASSConfig

-- | Represents Home Assistant services.
data Service = MkService
  { serviceName :: Text,
    serviceDescription :: Text,
    serviceFields :: M.Map Text Value
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "service") Service

-- | Represents Home Assistant service domains.
data ServiceDomain = MkServiceDomain
  { -- | The domain name.
    sdDomain :: Text,
    -- | The services in this domain as a mapping from names to services.
    sdServices :: M.Map Text Service
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (JSONOptions "sd") ServiceDomain

--------------------------------------------------------------------------------
