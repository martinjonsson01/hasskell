module Hasskell.HomeAssistant.Version
  ( HASSVersion (..),
    minimumSupportedVersion,
  )
where

import Data.Aeson
import Data.Aeson.Text
import Data.Char
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Prettyprinter

-- | The minimum version of Home Assistant that is compatible with Hasskell.
minimumSupportedVersion :: HASSVersion
minimumSupportedVersion =
  Version
    { year = 2025,
      month = 12,
      day = 0,
      modifierType = Nothing,
      modifierNum = Nothing
    }

-- | The version of the connected Home Assistant instance.
data HASSVersion = Version
  { year :: Int,
    month :: Int,
    day :: Int,
    modifierType :: Maybe Text,
    modifierNum :: Maybe Int
  }
  deriving (Eq, Show)

instance Ord HASSVersion where
  compare
    (Version year1 month1 day1 modifierType1 modifierNumber1)
    (Version year2 month2 day2 modifierType2 modifierNumber2) =
      compare (year1, month1, day1) (year2, month2, day2)
        <> compareModifier modifierType1 modifierNumber1 modifierType2 modifierNumber2

compareModifier ::
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Int ->
  Ordering
compareModifier Nothing _ Nothing _ = EQ
compareModifier Nothing _ (Just _) _ = GT -- release > any modifier
compareModifier (Just _) _ Nothing _ = LT -- any modifier < release
compareModifier (Just type1) number1 (Just type2) number2 =
  compare type1 type2 <> compare (fromMaybe 0 number1) (fromMaybe 0 number2)

instance FromJSON HASSVersion where
  parseJSON = withText "HASSVersion" $ \txt ->
    case parseVersionText txt of
      Just version -> pure version
      Nothing -> fail $ "Invalid HASS version string: " ++ T.unpack txt

instance ToJSON HASSVersion where
  toJSON (Version year month day modifierType modifiedNumber) =
    let base = show year ++ "." ++ show month ++ "." ++ show day
        modStr = case (modifierType, modifiedNumber) of
          (Just t, Just n) -> T.unpack t ++ show n
          _ -> ""
     in String $ T.pack (base ++ modStr)

parseVersionText :: Text -> Maybe HASSVersion
parseVersionText text = do
  let isDateChar c = isDigit c || c == '.'
      (datePart, modifierPart) = T.span isDateChar text

  [year, month, day] <- mapM decodeStrictText $ T.splitOn "." datePart

  let (modifierTypeText, modifierNumberText) = T.span isLetter modifierPart
      modifierType = nonEmpty modifierTypeText
      modifierNumber = nonEmpty modifierNumberText >>= decodeStrictText

  pure $ Version year month day modifierType modifierNumber
  where
    nonEmpty :: Text -> Maybe Text
    nonEmpty t = if T.null t then Nothing else Just t

instance Pretty HASSVersion where
  pretty = pretty . TL.toStrict . encodeToLazyText
