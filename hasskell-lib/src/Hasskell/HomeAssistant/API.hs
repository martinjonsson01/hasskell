{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Types used by Home Assistant.
module Hasskell.HomeAssistant.API
  ( HASSAuthMessage (..),
    HASSAuthResponse (..),
    Envelope (..),
    HASSCommand (..),
    HASSCommandGetEntityRegistry (..),
    HASSCommandGetDeviceRegistry (..),
    HASSResponse (..),
    HASSResult (..),
    HASSFailure (..),
    HASSConfig (..),
    EntityId (..),
    KnownEntityId,
    makeKnownEntityIdUnsafe,
    unwrapKnownEntityId,
    HASSEntity (..),
    HASSDevice (..),
    HASSState (..),
    HASSActionResult (..),
    HASSUnitSystem (..),
    HASSTrigger (..),
    HASSPlatform,
    HASSStateValue,
    HASSEvent (..),
    HASSVariables (..),
    HASSTriggered (..),
    -- Targeting
    HASSTarget (..),
    targetEntity,
    -- Domains
    HASSDomain,
    domainLight,
    domainInputBoolean,
    -- Services
    HASSService (..),
    HASSServiceActions,
    HASSServiceName,
    serviceToggle,
    serviceTurnOn,
    serviceTurnOff,
    HASSQualifiedServiceName,
    splitQualifiedServiceName,
  )
where

--------------------------------------------------------------------------------

import Control.Placeholder
import Data.Aeson
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (typeMismatch)
import Data.Char (toLower)
import Data.Coerce
import Data.Foldable (toList)
import Data.Hashable
import Data.List qualified as L
import Data.Map.Lazy qualified as M
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Deriving.Aeson
import GHC.TypeLits
import Hasskell.Effects.Counter (CorrelationId (..))
import Hasskell.HomeAssistant.Version
import Network.WebSockets qualified as WS
import Prettyprinter

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
     FieldLabelModifier '[StripPrefix prefix, CamelToSnake],
     ConstructorTagModifier '[Decapitalize, StripPrefix prefix, CamelToSnake]
   ]

--------------------------------------------------------------------------------

-- | Responses sent from Home Assistant in the authentication phase.
data HASSAuthResponse
  = ResponseAuthRequired {responseHaVersion :: HASSVersion}
  | ResponseAuthInvalid {responseMessage :: Text}
  | ResponseAuthOk {responseHaVersion :: HASSVersion}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "response") HASSAuthResponse

-- | Messages to send to Home Assistant during the authentication phase.
data HASSAuthMessage = MessageAuth {messageAccessToken :: Text}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "message") HASSAuthMessage

instance WS.WebSocketsData HASSAuthMessage where
  fromLazyByteString = unimplemented
  fromDataMessage = unimplemented
  toLazyByteString = encode

-------------------------------------------------------------------------------

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
  fromLazyByteString = unimplemented
  fromDataMessage = unimplemented
  toLazyByteString = encode

-------------------------------------------------------------------------------

data HASSCommandGetEntityRegistry = CommandGetEntityRegistry
  deriving (Eq, Show)

instance ToJSON HASSCommandGetEntityRegistry where
  toJSON CommandGetEntityRegistry = object ["type" .= ("config/entity_registry/list" :: Text)]

data HASSCommandGetDeviceRegistry = CommandGetDeviceRegistry
  deriving (Eq, Show)

instance ToJSON HASSCommandGetDeviceRegistry where
  toJSON CommandGetDeviceRegistry = object ["type" .= ("config/device_registry/list" :: Text)]

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
        commandTarget :: HASSTarget,
        commandReturnResponse :: Bool
      }
  | -- | Subscribes to state changes of the given entity.
    CommandSubscribeTrigger
      { commandTrigger :: HASSTrigger
      }
  | -- | Gets the supported services of a target.
    CommandGetServicesForTarget {commandTarget :: HASSTarget}
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSMessageJSONOptions "command") HASSCommand

instance WS.WebSocketsData HASSCommand where
  fromLazyByteString = unimplemented
  fromDataMessage = unimplemented
  toLazyByteString = encode

type HASSPlatform = Text

type HASSStateValue = Text

-- | A condition for an automation to run.
data HASSTrigger = Trigger
  { triggerPlatform :: HASSPlatform,
    triggerEntityId :: KnownEntityId,
    triggerFrom :: Maybe HASSStateValue,
    triggerTo :: Maybe HASSStateValue
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "trigger") HASSTrigger

data HASSTarget = Target
  { targetEntityId :: [KnownEntityId],
    targetDeviceId :: [Text],
    targetAreaId :: [Text],
    targetLabelId :: [Text]
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "target") HASSTarget

-- | Target a single entity.
targetEntity :: KnownEntityId -> HASSTarget
targetEntity eId =
  Target
    { targetEntityId = L.singleton eId,
      targetDeviceId = mempty,
      targetLabelId = mempty,
      targetAreaId = mempty
    }

data HASSResponse = ResponseResult (HASSResult Value) | ResponseEvent HASSEvent
  deriving (Eq, Show)

instance FromJSON HASSResponse where
  parseJSON = withObject "HASSResponse" $ \o -> do
    t :: String <- o .: "type"
    case t of
      "result" -> ResponseResult <$> parseJSON (Object o)
      "event" -> o .: "event" >>= (ResponseEvent <$>) . parseJSON
      other -> fail $ "Expected type=result|event, got: " ++ show other

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
  { stateEntityId :: KnownEntityId,
    stateState :: HASSStateValue,
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

-- | A data type used for parsing unix epoch time encoded UTC time.
newtype UnixUTC = UnixUTC UTCTime
  deriving (Show, Eq)

instance FromJSON UnixUTC where
  parseJSON v = parseJSON v >>= pure . UnixUTC . posixSecondsToUTCTime

instance ToJSON UnixUTC where
  toJSON (UnixUTC t) = toJSON (utcTimeToPOSIXSeconds t)

-- | An entity ID that may or may not refer to a real entity.
newtype EntityId = EntityId {unwrapEntityId :: Text}
  deriving (Show, Eq, Ord)
  deriving (FromJSON, ToJSON, Hashable) via Text

-- | An entity ID that has been confirmed to refer to an existing entity.
newtype KnownEntityId = Known {unwrapKnownEntityId :: EntityId}
  deriving (Eq, Ord, Show)
  deriving (Pretty) via EntityId
  deriving (FromJSON, ToJSON, Hashable) via Text

-- | Creates an entity ID without any verification that it is valid.
makeKnownEntityIdUnsafe :: EntityId -> KnownEntityId
makeKnownEntityIdUnsafe = coerce

instance Pretty EntityId where
  pretty (EntityId eId) = enclose backtick backtick (pretty eId)
    where
      backtick = pretty ("`" :: Text)

-- | Represents a Home Assistant entity.
data HASSEntity = MkEntity
  { entityEntityId :: KnownEntityId,
    entityPlatform :: Text,
    entityUniqueId :: Text,
    entityCategories :: KeyMap Value,
    entityHasEntityName :: Bool,
    entityCreatedAt :: UnixUTC,
    entityModifiedAt :: UnixUTC,
    entityOptions :: KeyMap Value,
    entityLabels :: [Text],
    entityDeviceId :: Maybe Text,
    entityConfigEntryId :: Maybe Text,
    entityAreaId :: Maybe Text,
    entityConfigSubentryId :: Maybe Text,
    entityDisabledBy :: Maybe Text,
    entityEntityCategory :: Maybe Text,
    entityHiddenBy :: Maybe Text,
    entityIcon :: Maybe Text,
    entityId :: Maybe Text,
    entityName :: Maybe Text,
    entityOriginalName :: Maybe Text,
    entityTranslationKey :: Maybe Text
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "entity") HASSEntity

-- | Represents a Home Assistant device software version.
data HASSSoftwareVersion = MkSwVersion [Text]
  deriving (Show, Eq, Generic)

instance FromJSON HASSSoftwareVersion where
  parseJSON Null = pure (MkSwVersion [])
  parseJSON (String t) = pure (MkSwVersion [t])
  parseJSON (Array arr) = MkSwVersion <$> mapM parseJSON (toList arr)
  parseJSON v = typeMismatch "MkSwVersion" v

instance ToJSON HASSSoftwareVersion where
  toJSON (MkSwVersion []) = Null
  toJSON (MkSwVersion [t]) = String t
  toJSON (MkSwVersion ts) = toJSON ts

-- | Represents a Home Assistant device.
data HASSDevice = MkDevice
  { deviceName :: Text,
    deviceId :: Text,
    deviceModifiedAt :: UnixUTC,
    deviceCreatedAt :: UnixUTC,
    deviceSwVersion :: HASSSoftwareVersion,
    deviceLabels :: [Text],
    deviceConfigEntries :: [Text],
    deviceConnections :: [[Text]],
    deviceIdentifiers :: [[Text]],
    deviceConfigEntriesSubentries :: KeyMap Value,
    devicePrimaryConfigEntry :: Maybe Text,
    deviceAreaId :: Maybe Text,
    deviceConfigurationUrl :: Maybe Text,
    deviceDisabledBy :: Maybe Text,
    deviceEntryType :: Maybe Text,
    deviceHwVersion :: Maybe Text,
    deviceManufacturer :: Maybe Text,
    deviceModel :: Maybe Text,
    deviceModelId :: Maybe Text,
    deviceNameByUser :: Maybe Text,
    deviceSerialNumber :: Maybe Text,
    deviceViaDeviceId :: Maybe Text
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "device") HASSDevice

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
  deriving (Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

serviceToggle :: HASSServiceName
serviceToggle = ServiceName "toggle"

serviceTurnOn :: HASSServiceName
serviceTurnOn = ServiceName "turn_on"

serviceTurnOff :: HASSServiceName
serviceTurnOff = ServiceName "turn_off"

-- | A service name with a domain prefix: `domain.service_name`
newtype HASSQualifiedServiceName = QualifiedServiceName Text
  deriving (Eq, Show, Ord)
  deriving (FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

splitQualifiedServiceName :: HASSQualifiedServiceName -> Maybe (HASSDomain, HASSServiceName)
splitQualifiedServiceName (QualifiedServiceName text) = do
  let (domain, rest) = T.break (== '.') text
  (_, service) <- T.uncons rest
  pure (Domain domain, ServiceName service)

newtype HASSDomain = Domain Text
  deriving (Eq, Show, Ord)
  deriving (Hashable, Pretty, FromJSON, ToJSON, FromJSONKey, ToJSONKey) via Text

domainLight :: HASSDomain
domainLight = Domain "light"

domainInputBoolean :: HASSDomain
domainInputBoolean = Domain "input_boolean"

type HASSServiceActions = M.Map HASSDomain (M.Map HASSServiceName HASSService)

-- | Represents Home Assistant services.
data HASSService = Service
  { serviceName :: Maybe Text,
    serviceDescription :: Maybe Text,
    serviceFields :: M.Map Text Value
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "service") HASSService

-- | An event emitted by a subscription.
data HASSEvent = Event
  { eventVariables :: HASSVariables
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "event") HASSEvent

-- | Variables associated with an event.
data HASSVariables = Variables
  { variablesTrigger :: HASSTriggered
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "variables") HASSVariables

-- | The result of a Trigger firing.
data HASSTriggered = Triggered
  { triggeredId :: Text,
    triggeredIdx :: Text,
    triggeredAlias :: Maybe Text,
    triggeredPlatform :: Text,
    triggeredEntityId :: KnownEntityId,
    triggeredFor :: Maybe Text,
    triggeredAttribute :: Maybe Text,
    triggeredDescription :: Text,
    triggeredFromState :: HASSState,
    triggeredToState :: HASSState
  }
  deriving (Generic, Eq, Show)
  deriving (FromJSON, ToJSON) via CustomJSON (HASSValueJSONOptions "triggered") HASSTriggered
