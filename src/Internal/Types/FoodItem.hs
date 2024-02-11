{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.FoodItem
  ( ParsedFoodItem
  , MappedFoodItem
  , NutrientMap
  , FoodItem (..)
  , FoodNutrient (..)
  , Nutrient (..)
  , ValidNutrient (..)
  , FID (..)
  , NID (..)
  , Mass (..)
  , ProteinConversion (..)
  , Unit (..)
  , Measurement (..)
  , Prefix (..)
  , prefixSymbol
  , measurementSymbol
  , tunit
  )
where

import Data.Aeson
import Data.Aeson.Types
import Data.Csv (ToField, toField)
import Data.Scientific
import GHC.Generics
import Internal.Types.Dhall
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Text as T

-- | Food item with its nutrients validated and put into a convenient map
type MappedFoodItem = FoodItem NutrientMap

-- | Food item freshly parsed from JSON
type ParsedFoodItem = FoodItem [FoodNutrient]

type NutrientMap = Map NID ValidNutrient

-- | Nutrient that has been validated (which means it has an ID, non-negative
-- mass, unit, etc)
data ValidNutrient = ValidNutrient
  { vnAmount :: Mass
  , vnPrefix :: Prefix
  , vnName :: Maybe Text
  }
  deriving (Show, Eq)

-- TODO filter out nutrients that aren't analytical (or similar)
-- TODO need a way to filter out/warn user on bad nutrient data
data FoodItem n = FoodItem
  { fiDescription :: Text
  , fiFoodNutrients :: n
  , fiCalorieConversion :: CalorieConversion
  , fiProteinConversion :: ProteinConversion
  }
  deriving (Generic, Show)

instance FromJSON ParsedFoodItem where
  parseJSON (Object v) = do
    t <- v .: "dataType"
    if (t :: Text) `elem` ["Branded", "Foundation", "SR Legacy"]
      then parseFoodItem v
      else mempty
  parseJSON _ = mempty

parseFoodItem :: Object -> Parser ParsedFoodItem
parseFoodItem v = do
  ncf <- v .: "nutrientConversionFactors"
  c <- firstM parseCalorieConversion ncf
  p <- firstM parseProteinConversion ncf
  FoodItem
    <$> v .: "description"
    <*> (v .:? "foodNutrients" .!= [])
    <*> pure (fromMaybe defCalorie c)
    <*> pure (fromMaybe defProtein p)
  where
    defCalorie = CalorieConversion {ccFat = 9, ccProtein = 4, ccCarbs = 4}
    defProtein = ProteinConversion 6.25

parseCalorieConversion :: Object -> Parser (Maybe CalorieConversion)
parseCalorieConversion v = do
  t <- v .: "type"
  case (t :: Text) of
    ".CalorieConversionFactor" -> do
      f <- v .: "fatValue"
      p <- v .: "proteinValue"
      c <- v .: "carbohydrateValue"
      return $ Just $ CalorieConversion f p c
    _ -> return Nothing

parseProteinConversion :: Object -> Parser (Maybe ProteinConversion)
parseProteinConversion v = do
  t <- v .: "type"
  case (t :: Text) of
    ".ProteinConversionFactor" -> Just <$> v .: "value"
    _ -> return Nothing

firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstM _ [] = return Nothing
firstM f (x : xs) = maybe (firstM f xs) (return . Just) =<< f x

data FoodNutrient = FoodNutrient
  { fnNutrient :: Maybe Nutrient
  , fnAmount :: Maybe Mass
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrient where
  parseJSON = recordParseJSON "fn"

data Nutrient = Nutrient
  { nId :: Maybe NID
  , nName :: Maybe Text
  , nUnitName :: Maybe Text
  }
  deriving (Generic, Show)

instance FromJSON Nutrient where
  parseJSON = recordParseJSON "n"

newtype FID = FID {unFID :: Natural}
  deriving (Eq, Read, Show, FromJSON, ToJSON) via Natural

newtype NID = NID {unNID :: Natural}
  deriving (Read, Show, FromJSON, ToJSON, Eq, Ord, Num, ToField) via Natural

recordParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
recordParseJSON s = genericParseJSON (recordOptions s)

stripRecordPrefix :: String -> String -> String
stripRecordPrefix prefix = maybe [] go . L.stripPrefix prefix
  where
    go [] = []
    go (x : xs) = C.toLower x : xs

recordOptions :: String -> Options
recordOptions x =
  defaultOptions
    { fieldLabelModifier = stripRecordPrefix x
    , rejectUnknownFields = False
    }

--------------------------------------------------------------------------------
-- units

data Measurement
  = Gram
  | Calorie
  deriving (Show, Eq, Ord, Generic, ToJSON)

data Unit = Unit
  { unitPrefix :: Prefix
  , unitMeasurement :: Measurement
  }
  deriving (Show, Eq, Generic, ToJSON)

prefixSymbol :: Prefix -> Text
prefixSymbol Nano = "n"
prefixSymbol Micro = "μ"
prefixSymbol Milli = "m"
prefixSymbol Centi = "c"
prefixSymbol Deci = "d"
prefixSymbol Unity = ""
prefixSymbol Deca = "da"
prefixSymbol Hecto = "h"
prefixSymbol Kilo = "k"
prefixSymbol Mega = "M"
prefixSymbol Giga = "G"

measurementSymbol :: Measurement -> Text
measurementSymbol Calorie = "cal"
measurementSymbol Gram = "g"

tunit :: Unit -> Text
tunit (Unit p n) = T.append (prefixSymbol p) (measurementSymbol n)

instance ToField Unit where
  toField = encodeUtf8 . tunit

data Prefix
  = Nano
  | Micro
  | Milli
  | Centi
  | Deci
  | Unity
  | Deca
  | Hecto
  | Kilo
  | Mega
  | Giga
  deriving (Show, Eq, Ord, Generic, ToJSON, Read, Bounded)

newtype Mass = Mass {unMass :: Scientific}
  deriving
    ( Read
    , Show
    , FromJSON
    , ToJSON
    , Eq
    , Ord
    , Num
    , Fractional
    , Real
    , ToField
    , RealFrac
    )
    via Scientific

newtype ProteinConversion = ProteinConversion {unPC :: Scientific}
  deriving (Read, Show, FromJSON, ToJSON, Eq, Ord, Num) via Scientific
