{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.FoodItem where

import Data.Aeson
import Data.Aeson.Types
import Data.Scientific
import GHC.Generics
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L

data FoodItem
  = -- = Branded BrandedFoodItem
    Foundation FoundationFoodItem
  | SRLegacy SRLegacyFoodItem
  deriving (Show, Generic)

instance FromJSON FoodItem where
  parseJSON o@(Object v) = do
    t <- v .: "dataType"
    go t
    where
      go :: Text -> Parser FoodItem
      go t
        -- \| t == "Branded" = Branded <$> parseJSON o
        | t == "Branded" = mempty
        | t == "Foundation" = Foundation <$> parseJSON o
        | t == "Survey (FNDDS)" = mempty
        | t == "SR Legacy" = SRLegacy <$> parseJSON o
        | otherwise = mempty
  parseJSON _ = mempty

data BrandedFoodItem = BrandedFoodItem
  { bfiMeta :: FoodRequiredMeta
  , bfiCommon :: FoodCommon
  , bfiBrandOwner :: Text
  , bfiDataSource :: Maybe Text
  , -- , bfiGtinUpc :: Maybe Text
    -- , bfiHouseholdServingFullText :: Maybe Text
    bfiIngredients :: Maybe Text
  , -- , bfiModifiedDate :: Maybe Text
    bfiServingSize :: Maybe Int
  , -- , bfiFormat :: Maybe Double
    bfiServingSizeUnit :: Maybe Text
    -- , bfiPreparationStateCode :: Maybe Text
    -- , bfiBrandedFoodCategory :: Maybe Text
    -- , bfiTradeChannel :: [Text]
    -- , bfiGpcClassCode :: Maybe Int
    -- , bfiFoodUpdateLog :: Maybe [FoodUpdateLog]
    -- , bfiLabelNutrients :: Maybe [LabelNutrient]
  }
  deriving (Show, Generic)

instance FromJSON BrandedFoodItem where
  parseJSON = withObject "BrandedFoodItem" $ \v ->
    BrandedFoodItem
      <$> parseFoodRequiredMeta v
      <*> parseFoodCommon v
      <*> v
        .: "brandOwner"
      <*> v
        .: "dataSource"
      <*> v
        .: "ingredients"
      <*> v
        .: "servingSize"
      <*> v
        .: "servingSizeUnit"

parseFoodRequiredMeta :: Object -> Parser FoodRequiredMeta
parseFoodRequiredMeta v =
  FoodRequiredMeta
    <$> v
      .: "fdcId"
    <*> v
      .: "description"

parseFoodCommon :: Object -> Parser FoodCommon
parseFoodCommon v =
  FoodCommon
    <$> v .:? "foodClass"
    <*> v .:? "publicationDate"
    <*> (v .:? "foodNutrients" .!= [])

data FoundationFoodItem = FoundationFoodItem
  { ffiMeta :: FoodRequiredMeta
  , ffiCommon :: FoundationLegacyCommon
  , ffiFootNote :: Maybe Text
  , ffiFoodComponents :: Maybe [FoodComponent]
  , ffiFoodAttributes :: Maybe [FoodAttribute]
  , ffiFoodPortions :: Maybe [FoodPortion]
  , ffiInputFoods :: Maybe [InputFoodFoundation]
  }
  deriving (Show, Generic)

instance FromJSON FoundationFoodItem where
  parseJSON = withObject "FoundationFoodItem" $ \v ->
    FoundationFoodItem
      <$> parseFoodRequiredMeta v
      <*> parseFoundationLegacyCommon v
      <*> v .:? "foodNote"
      <*> v .:? "foodComponents"
      <*> v .:? "foodAttributes"
      <*> v .:? "foodPortions"
      <*> v .:? "inputFoods"

data SRLegacyFoodItem = SRLegacyFoodItem
  { srlMeta :: FoodRequiredMeta
  , srlCommon :: FoundationLegacyCommon
  }
  deriving (Show, Generic)

instance FromJSON SRLegacyFoodItem where
  parseJSON = withObject "SRLegacyFoodItem" $ \v ->
    SRLegacyFoodItem
      <$> parseFoodRequiredMeta v
      <*> parseFoundationLegacyCommon v

data FoodRequiredMeta = FoodRequiredMeta
  { frmId :: FID
  , frmDescription :: Text
  }
  deriving (Show)

data FoodCommon = FoodCommon
  { fcFoodClass :: Maybe Text
  , fcPublicationDate :: Maybe Text
  , fcFoodNutrients :: [FoodNutrient]
  }
  deriving (Show)

data FoundationLegacyCommon = FoundationLegacyCommon
  { flcCommon :: FoodCommon
  , flcIsHistoricalReference :: Maybe Bool
  , flcNdbNumber :: Maybe Int
  , flcScientificName :: Maybe Text
  , flcFoodCategory :: Maybe FoodCategory
  , flcCalorieConversion :: CalorieConversion
  , flcProteinConversion :: ProteinConversion
  }
  deriving (Show)

data CalorieConversion = CalorieConversion
  { ccFat :: Scientific
  , ccProtein :: Scientific
  , ccCarbs :: Scientific
  , ccAssumed :: Bool
  }
  deriving (Show)

newtype ProteinConversion = ProteinConversion
  { pcFactor :: Scientific
  }
  deriving (Show)

parseFoundationLegacyCommon :: Object -> Parser FoundationLegacyCommon
parseFoundationLegacyCommon v = do
  ncf <- v .: "nutrientConversionFactors"
  c <- firstM parseCalorieConversion ncf
  p <- firstM parseProteinConversion ncf
  FoundationLegacyCommon
    <$> parseFoodCommon v
    <*> v .:? "isHistoricalReference"
    <*> v .:? "ndbNumber"
    <*> v .:? "scientificName"
    <*> v .:? "foodCategory"
    <*> pure (fromMaybe defCalorie c)
    <*> pure (fromMaybe defProtein p)
  where
    defCalorie = CalorieConversion {ccFat = 9, ccProtein = 4, ccCarbs = 4, ccAssumed = True}
    defProtein = ProteinConversion 6.25

parseCalorieConversion :: Object -> Parser (Maybe CalorieConversion)
parseCalorieConversion v = do
  t <- v .: "type"
  case (t :: Text) of
    ".CalorieConversionFactor" -> do
      f <- v .: "fatValue"
      p <- v .: "proteinValue"
      c <- v .: "carbohydrateValue"
      return $ Just $ CalorieConversion f p c False
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
  , lnCarbohydrates_ :: Maybe Scientific
  , lnFiber :: Maybe Scientific
  , lnSugars_ :: Maybe Scientific
  , lnProtein :: Maybe Scientific
  , lnCalcium :: Maybe Scientific
  , lnIron :: Maybe Scientific
  , lnPotassium :: Maybe Scientific
  , lnCalories :: Maybe Scientific
  }
  deriving (Show, Generic)

instance FromJSON LabelNutrient where
  parseJSON = recordParseJSON "ln"

data FoodCategory = FoodCategory
  { fcId :: Maybe Int
  , fcCode :: Maybe Text
  , fcDescription :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON FoodCategory where
  parseJSON = recordParseJSON "fc"

data FoodComponent = FoodComponent
  deriving (Show, Generic, FromJSON)

data FoodPortion = FoodPortion
  { fpId :: Maybe Int
  , fpAmount :: Maybe Scientific
  , fpDataPoints :: Maybe Int
  , fpGramWeight :: Maybe Scientific
  , fpMinYearAcquired :: Maybe Int
  , fpMinDateAcquired :: Maybe Text
  , fpModifier :: Maybe Text
  , fpPortionDescription :: Maybe Text
  , fpSequenceNumber :: Maybe Int
  , fpMeasureUnit :: Maybe MeasureUnit
  }
  deriving (Show, Generic)

instance FromJSON FoodPortion where
  parseJSON = recordParseJSON "fp"

data MeasureUnit = MeausureUnit
  { muId :: Maybe Int
  , muAbbreviation :: Maybe Text
  , muName :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON MeasureUnit where
  parseJSON = recordParseJSON "mu"

data InputFoodFoundation = InputFoodFoundation
  { iffId :: Maybe Int
  , iffFoodDescription :: Maybe Text
  , iffInputFood :: Maybe SampleFoodItem
  }
  deriving (Show, Generic)

instance FromJSON InputFoodFoundation where
  parseJSON = recordParseJSON "iff"

-- TODO missing foodGroup, foodAttributeTypes, totalRefuse
data SampleFoodItem = SampleFoodItem
  { sfiFdcId :: FID
  , sfiDescription :: Text
  , sfiFoodClass :: Maybe Text
  , sfiPublicationDate :: Maybe Text
  , sfiFoodAttributes :: Maybe [Text]
  }
  deriving (Show, Generic)

instance FromJSON SampleFoodItem where
  parseJSON = recordParseJSON "sfi"

data NutrientConversionFactor = NutrientConversionFactor
  { ncfType :: Maybe Text
  , ncfValue :: Maybe Scientific
  }
  deriving (Show, Generic)

instance FromJSON NutrientConversionFactor where
  parseJSON = recordParseJSON "ncf"

-- NOTE all of these are maybe (unlike what the doc says)
data FoodNutrient = FoodNutrient
  { fnId :: Maybe Int
  , fnNutrient :: Maybe Nutrient
  , fnAmount :: Maybe Scientific
  , fnDataPoints :: Maybe Integer
  , fnMin :: Maybe Scientific
  , fnMax :: Maybe Scientific
  , fnMedian :: Maybe Scientific
  , fnFoodNutrientDerivation :: Maybe NutrientDerivation
  , fnNutrientAnalysisDetails :: [NutrientAnalysisDetails]
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrient where
  parseJSON = withObject "FoodNutrient" $ \v ->
    FoodNutrient
      <$> v .:? "id"
      <*> v .:? "nutrient"
      <*> v .:? "amount"
      <*> v .:? "dataPoints"
      <*> v .:? "min"
      <*> v .:? "max"
      <*> v .:? "median"
      <*> v .:? "foodNutrientDerivation"
      <*> v .:? "nutrientAnalysisDetails" .!= []

data NutrientDerivation = NutrientDerivation
  { ndId :: Maybe Int
  , ndCode :: Maybe Text
  , ndDescription :: Maybe Text
  , ndFoodNutrientSource :: Maybe FoodNutrientSource
  }
  deriving (Show, Generic)

instance FromJSON NutrientDerivation where
  parseJSON = recordParseJSON "nd"

data Nutrient = Nutrient
  { nId :: Maybe NID
  , nNumber :: Maybe Text
  , nName :: Maybe Text
  , nRank :: Maybe Int
  , nUnitName :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Nutrient where
  parseJSON = recordParseJSON "n"

data FoodNutrientDerivation = FoodNutrientDerivation
  { fndId :: Maybe Int
  , fndCode :: Maybe Text
  , fndDescription :: Maybe Text
  , fndFoodNutientSource :: Maybe FoodNutrientSource
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrientDerivation where
  parseJSON = recordParseJSON "fnd"

data FoodNutrientSource = FoodNutrientSource
  { fnsId :: Maybe Int
  , fnsCode :: Maybe Text
  , fnsDescription :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrientSource where
  parseJSON = recordParseJSON "fns"

data NutrientAnalysisDetails = NutrientAnalysisDetails
  { nadSubSampleId :: Maybe Int
  , nadAmount :: Maybe Scientific
  , -- , nadNutrientId :: Maybe Int
    nadLabMethodDescription :: Maybe Text
  , nadLabMethodOriginalDescription :: Maybe Text
  , -- , nadLabMethodLink :: Maybe Text
    nadLabMethodTechnique :: Maybe Text
    -- , nadNutrientAcquisitionDetails :: Maybe [NutrientAcquisitionDetails]
  }
  deriving (Show, Generic)

instance FromJSON NutrientAnalysisDetails where
  parseJSON = recordParseJSON "nad"

-- data NutrientAcquisitionDetails = NutrientAcquisitionDetails
--   { ncdSampleUnitId :: Maybe Int
--   , ncdPurchaseDate :: Maybe Text
--   , ncdStoreCity :: Maybe Text
--   , ncdStoreState :: Maybe Text
--   }
--   deriving (Show, Generic)

-- instance FromJSON NutrientAcquisitionDetails where
--   parseJSON = recordParseJSON "ncd"

data FoodUpdateLog = FoodUpdateLog
  { fulFdcId :: Maybe FID
  , fulAvailableDate :: Maybe Text
  , fulBrandOwner :: Maybe Text
  , fulDataSource :: Maybe Text
  , fulDescription :: Maybe Text
  , fulFoodClass :: Maybe Text
  , fulGtinUpc :: Maybe Text
  , fulHouseholdServingFullText :: Maybe Text
  , fulIngredients :: Maybe Text
  , fulModifiedDate :: Maybe Text
  , fulPublicationDate :: Maybe Text
  , fulServingSize :: Maybe Scientific
  , fulServingSizeUnit :: Maybe Text
  , fulBrandedFoodCategory :: Maybe Text
  , fulChanges :: Maybe Text
  , fulFoodAttributes :: [FoodAttribute]
  }
  deriving (Show, Generic)

instance FromJSON FoodUpdateLog where
  parseJSON = recordParseJSON "ful"

data FoodAttribute = FoodAttribute
  { faID :: Maybe Int
  , faSequenceNumber :: Maybe Int
  , faValue :: Maybe Text
  , faFoodAttributeType :: FoodAttributeType
  }
  deriving (Show, Generic)

instance FromJSON FoodAttribute where
  parseJSON = recordParseJSON "fa"

data FoodAttributeType = FoodAttributeType
  { fatID :: Maybe Int
  , fatName :: Maybe Text
  , fatDescription :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON FoodAttributeType where
  parseJSON = recordParseJSON "fat"

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
