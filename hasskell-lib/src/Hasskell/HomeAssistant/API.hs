{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Types used by Home Assistant.
module Hasskell.HomeAssistant.API
  ( HASSAuthMessage (..),
    HASSAuthResponse (..),
    Envelope (..),
    CorrelationId,
    HASSCommand (..),
    HASSResult (..),
    HASSFailure (..),
    HASSConfig (..),
    HASSState (..),
    HASSTarget (..),
    HASSActionResult (..),
    HASSDomain (..),
    HASSUnitSystem (..),
    HASSServiceActions,
    HASSService (..),
    HASSServiceName (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (toLower)
import Data.Map.Lazy qualified as M
import Data.Text (Text)
import Data.Time (UTCTime)
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

-- | The custom JSON encoding/decoding options for HASS message types.
--
-- HA does not like fields that are set to @null@ - they must be omitted.
type HASSMessageJSONOptions (prefix :: Symbol) =
  '[ OmitNothingFields,
     FieldLabelModifier '[StripPrefix prefix, CamelToSnake],
     TagSingleConstructors, -- Include tag field even for types with a single constructor.
     NoAllNullaryToStringTag, -- Include tag field even for types with no fields.
     SumTaggedObject "type" "",
     ConstructorTagModifier '[Decapitalize, StripPrefix prefix, CamelToSnake]
   ]

-- | Value data types are parsed differently than message data types, mainly due to them not being sum encoded.
type HASSValueJSONOptions (prefix :: Symbol) =
  '[ OmitNothingFields,
     UnwrapUnaryRecords,
     FieldLabelModifier '[StripPrefix prefix, CamelToSnake],
     ConstructorTagModifier '[Decapitalize, StripPrefix prefix, CamelToSnake]
   ]

--------------------------------------------------------------------------------

-- | Responses sent from Home Assistant in the authentication phase.
data HASSAuthResponse
  = ResponseAuthRequired {responseHaVersion :: Text}
  | ResponseAuthInvalid {responseMessage :: Text}
  | ResponseAuthOk {responseHaVersion :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "response") HASSAuthResponse

-- | Messages to send to Home Assistant during the authentication phase.
data HASSAuthMessage = MessageAuth {messageAccessToken :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "message") HASSAuthMessage

instance WS.WebSocketsData HASSAuthMessage where
  fromLazyByteString = undefined
  fromDataMessage = undefined
  toLazyByteString = encode

-------------------------------------------------------------------------------

-- | Unique ID associated with a sent message.
newtype CorrelationId = CorrelationId Int
  deriving newtype (Eq, Show, Num)

-- | A wrapper around a message, identifying it
-- such that the response to the message will be easy to identify.
data Envelope a = Envelope
  { envelopeId :: CorrelationId,
    envelopePayload :: a
  }
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (Envelope a) where
  parseJSON = withObject "Envelope" $ \o -> do
    i <- CorrelationId <$> o .: "id"
    -- parse the full object again as 'a'
    payload <- parseJSON (Object o)
    pure (Envelope i payload)

instance (ToJSON a) => ToJSON (Envelope a) where
  toJSON (Envelope (CorrelationId i) payload) =
    case toJSON payload of
      Object payloadObject -> Object (KeyMap.insert "id" (toJSON i) payloadObject)
      val -> error $ "Unknown envelope payload type: " ++ show val

instance (ToJSON a) => WS.WebSocketsData (Envelope a) where
  fromLazyByteString = undefined
  fromDataMessage = undefined
  toLazyByteString = encode

-------------------------------------------------------------------------------

-- | Commands that can be issued to Home Assistant.
data HASSCommand
  = -- | Dumps the current config.
    CommandGetConfig
  | -- | Dumps all the current states.
    CommandGetStates
  | -- | Dumps all the current service actions.
    CommandGetServices
  | -- | Calls a service action.
    CommandCallService
      { commandDomain :: HASSDomain,
        commandService :: HASSServiceName,
        commandServiceData :: Maybe (M.Map Text Text),
        commandTarget :: Maybe HASSTarget,
        commandReturnResponse :: Bool
      }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "command") HASSCommand

instance WS.WebSocketsData HASSCommand where
  fromLazyByteString = undefined
  fromDataMessage = undefined
  toLazyByteString = encode

data HASSTarget = Target
  { targetEntityId :: [Text],
    targetDeviceId :: [Text],
    targetAreaId :: [Text],
    targetLabelId :: [Text]
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "target") HASSTarget

data HASSResult a = Result {value :: Either HASSFailure a}
  deriving (Eq, Show)

instance (FromJSON a) => FromJSON (HASSResult a) where
  parseJSON = withObject "HASSResult" $ \o -> do
    t :: String <- o .: "type"
    if t /= "result"
      then fail $ "Expected type=result, got: " ++ show t
      else do
        success <- o .: "success"
        value <-
          if success
            then Right <$> o .: "result"
            else Left <$> o .: "error"
        return $ Result value

data HASSActionResult = ActionResult
  { actionResultContext :: HASSContext,
    actionResultResponse :: Maybe Value
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "actionResult") HASSActionResult

data HASSState = State
  { stateEntityId :: Text,
    stateState :: Text,
    stateAttributes :: KeyMap Value,
    stateLastChanged :: UTCTime,
    stateLastReported :: UTCTime,
    stateLastUpdated :: UTCTime,
    stateContext :: HASSContext
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "state") HASSState

data HASSContext = HASSContext
  { contextId :: Text,
    contextParentId :: Maybe Text,
    contextUserId :: Maybe Text
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "context") HASSContext

-- | A command failure response.
data HASSFailure = Failure {failureCode :: Text, failureMessage :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "failure") HASSFailure

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
    configUnitSystem :: HASSUnitSystem,
    configVersion :: Text,
    configWhitelistExternalDirs :: [Text]
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "config") HASSConfig

-- | Represents units that Home Assistant is configured to use.
data HASSUnitSystem = MkUnitSystem
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
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "unitSystem") HASSUnitSystem

newtype HASSServiceName = ServiceName Text
  deriving (Eq, Show, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

newtype HASSDomain = Domain Text
  deriving (Eq, Show, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

type HASSServiceActions = M.Map HASSDomain (M.Map HASSServiceName HASSService)

-- | Represents Home Assistant services.
data HASSService = Service
  { serviceName :: Text,
    serviceDescription :: Text,
    serviceFields :: M.Map Text Value
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "service") HASSService
