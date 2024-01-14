{-# LANGUAGE DerivingVia #-}

module Internal.Types.FoodItem where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import GHC.Generics
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L

data FoodItem = FoodItem
  { fiId :: FID
  , fiDescription :: Text
  , fiFoodNutrients :: [FoodNutrient]
  , fiCalorieConversion :: CalorieConversion
  , fiProteinConversion :: ProteinConversion
  }
  deriving (Generic)

instance FromJSON FoodItem where
  parseJSON (Object v) = do
    t <- v .: "dataType"
    go t
    where
      go :: Text -> Parser FoodItem
      go t
        | t == "Branded" = parseFoodItem v
        | t == "Foundation" = parseFoodItem v
        | t == "Survey (FNDDS)" = mempty
        | t == "SR Legacy" = parseFoodItem v
        | otherwise = mempty
  parseJSON _ = mempty

parseFoodItem :: Object -> Parser FoodItem
parseFoodItem v = do
  ncf <- v .: "nutrientConversionFactors"
  c <- firstM parseCalorieConversion ncf
  p <- firstM parseProteinConversion ncf
  FoodItem
    <$> v .: "fdcId"
    <*> v .: "description"
    <*> (v .:? "foodNutrients" .!= [])
    <*> pure (fromMaybe defCalorie c)
    <*> pure (fromMaybe defProtein p)
  where
    defCalorie = CalorieConversion {ccFat = 9, ccProtein = 4, ccCarbs = 4}
    defProtein = ProteinConversion 6.25

-- parseFoodCommon :: Object -> Parser FoodCommon
-- parseFoodCommon v =
--   FoodCommon
--     <$> v .:? "foodClass"
--     <*> v .:? "publicationDate"

-- instance FromJSON FoundationLegacyFoodItem where
--   parseJSON = withObject "FoundationFoodItem" $ \v ->
--     FoundationLegacyFoodItem
--       <$> parseFoodRequiredMeta v
--       <*> parseFoundationLegacyCommon v

-- data FoodRequiredMeta = FoodRequiredMeta
--   { frmId :: FID
--   , frmDescription :: Text
--   }
--   deriving (Show)

-- data FoodCommon = FoodCommon
--   { fcFoodClass :: Maybe Text
--   , fcPublicationDate :: Maybe Text
--   , fcFoodNutrients :: [FoodNutrient]
--   }
--   deriving (Show)

-- data FoundationLegacyCommon = FoundationLegacyCommon
--   { flcCommon :: FoodCommon
--   , flcCalorieConversion :: CalorieConversion
--   , flcProteinConversion :: ProteinConversion
--   }

data CalorieConversion = CalorieConversion
  { ccFat :: Scientific
  , ccProtein :: Scientific
  , ccCarbs :: Scientific
  }

newtype ProteinConversion = ProteinConversion
  { pcFactor :: Scientific
  }

-- parseFoundationLegacyCommon :: Object -> Parser FoundationLegacyCommon
-- parseFoundationLegacyCommon v = do
--   ncf <- v .: "nutrientConversionFactors"
--   c <- firstM parseCalorieConversion ncf
--   p <- firstM parseProteinConversion ncf
--   FoundationLegacyCommon
--     <$> parseFoodCommon v
--     <*> pure (fromMaybe defCalorie c)
--     <*> pure (fromMaybe defProtein p)
--   where
--     defCalorie = CalorieConversion {ccFat = 9, ccProtein = 4, ccCarbs = 4}
--     defProtein = ProteinConversion 6.25

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
    ".ProteinConversionFactor" -> do
      p <- v .: "value"
      return $ Just $ ProteinConversion p
    _ -> return Nothing

firstM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Maybe b)
firstM _ [] = return Nothing
firstM f (x : xs) = maybe (firstM f xs) (return . Just) =<< f x

data LabelNutrient = LabelNutrient
  { lnFat :: Maybe Scientific
  , lnSaturatedFat :: Maybe Scientific
  , lnTransFat :: Maybe Scientific
  , lnCholesterol :: Maybe Scientific
  , lnSodium :: Maybe Scientific
  , lnCarbohydrates :: Maybe Scientific
  , lnFiber :: Maybe Scientific
  , lnSugars :: Maybe Scientific
  , lnProtein :: Maybe Scientific
  , lnCalcium :: Maybe Scientific
  , lnIron :: Maybe Scientific
  , lnPotassium :: Maybe Scientific
  , lnCalories :: Maybe Scientific
  }
  deriving (Show, Generic)

instance FromJSON LabelNutrient where
  parseJSON = recordParseJSON "ln"

data FoodNutrient = FoodNutrient
  { fnNutrient :: Maybe Nutrient
  , fnAmount :: Maybe Scientific
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

newtype FID = FID {unFID :: Int}
  deriving (Eq, Read, Show, FromJSON, ToJSON) via Int

newtype NID = NID {unNID :: Int}
  deriving (Read, Show, FromJSON, ToJSON, Eq, Ord, Num) via Int

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
