{-# LANGUAGE DeriveAnyClass #-}

module Internal.Types.FDC where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Text as T

data FoodItem
  = Branded BrandedFoodItem
  | Foundation FoundationFoodItem
  | SRLegacy SRLegacyFoodItem
  | Survey SurveyFoodItem
  deriving (Show, Generic)

instance FromJSON FoodItem where
  parseJSON o@(Object v) = do
    t <- v .: "dataType"
    go t
    where
      go :: T.Text -> Parser FoodItem
      go t
        | t == "Branded" = Branded <$> parseJSON o
        | t == "Foundation" = Foundation <$> parseJSON o
        | t == "Survey (FNDDS)" = Survey <$> parseJSON o
        | t == "SR Legacy" = SRLegacy <$> parseJSON o
        | otherwise = mempty
  parseJSON _ = mempty

data BrandedFoodItem = BrandedFoodItem
  { bfiDataType :: T.Text
  , bfiDescription :: T.Text
  , bfiFdcId :: Int
  , bfiAvailableDate :: Maybe T.Text
  , bfiBrandOwner :: T.Text
  , bfiDataSource :: Maybe T.Text
  , bfiFoodClass :: Maybe T.Text
  , bfiGtinUpc :: Maybe T.Text
  , bfiHouseholdServingFullText :: Maybe T.Text
  , bfiIngredients :: Maybe T.Text
  , bfiModifiedDate :: Maybe T.Text
  , bfiPublicationDate :: Maybe T.Text
  , bfiServingSize :: Maybe Int
  , bfiFormat :: Maybe Double
  , bfiServingSizeUnit :: Maybe T.Text
  , bfiPreparationStateCode :: Maybe T.Text
  , bfiBrandedFoodCategory :: Maybe T.Text
  , bfiTradeChannel :: [T.Text]
  , bfiGpcClassCode :: Maybe Int
  , bfiFoodNutrients :: [FoodNutrient]
  , bfiFoodUpdateLog :: [FoodUpdateLog]
  , bfiLabelNutrients :: [LabelNutrient]
  }
  deriving (Show, Generic)

instance FromJSON BrandedFoodItem where
  parseJSON = recordParseJSON "bfi"

data LabelNutrient = LabelNutrient
  { lnFat :: Maybe Double
  , lnSaturatedFat :: Maybe Double
  , lnTransFat :: Maybe Double
  , lnCholesterol :: Maybe Double
  , lnSodium :: Maybe Double
  , lnCarbohydrates :: Maybe Double
  , lnFiber :: Maybe Double
  , lnSugars :: Maybe Double
  , lnProtein :: Maybe Double
  , lnCalcium :: Maybe Double
  , lnIron :: Maybe Double
  , lnPotassium :: Maybe Double
  , lnCalories :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON LabelNutrient where
  parseJSON = recordParseJSON "ln"

data FoundationFoodItem = FoundationFoodItem
  { ffiFdcId :: Int
  , ffiDataType :: T.Text
  , ffiDescription :: T.Text
  , ffiFoodClass :: Maybe T.Text
  , ffiFootNote :: Maybe T.Text
  , ffiIsHistoricalReference :: Maybe Bool
  , ffiNdbNumber :: Maybe Int
  , ffiPublicationDate :: Maybe T.Text
  , ffiScientificName :: Maybe T.Text
  , ffiFoodCategory :: Maybe FoodCategory
  , ffiFoodComponents :: Maybe [FoodComponent]
  , ffiFoodNutrients :: Maybe [FoodNutrient]
  , ffiFoodAttributes :: Maybe [FoodAttribute]
  , ffiFoodPortions :: Maybe [FoodPortion]
  , ffiInputFoods :: Maybe [InputFoodFoundation]
  , ffiNutrientConversionFactors :: Maybe [NutrientConversionFactor]
  }
  deriving (Show, Generic)

instance FromJSON FoundationFoodItem where
  parseJSON = recordParseJSON "ffi"

data FoodCategory = FoodCategory
  { fcId :: Maybe Int
  , fcCode :: Maybe T.Text
  , fcDescription :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON FoodCategory where
  parseJSON = recordParseJSON "fc"

data SRLegacyFoodItem = SRLegacyFoodItem
  deriving (Show, Generic, FromJSON)

data SurveyFoodItem = SurveyFoodItem
  deriving (Show, Generic, FromJSON)

data FoodComponent = FoodComponent
  deriving (Show, Generic, FromJSON)

data FoodPortion = FoodPortion
  { fpId :: Maybe Int
  , fpAmount :: Maybe Double
  , fpDataPoints :: Maybe Int
  , fpGramWeight :: Maybe Double
  , fpMinYearAcquired :: Maybe Int
  , fpMinDateAcquired :: Maybe T.Text
  , fpModifier :: Maybe T.Text
  , fpPortionDescription :: Maybe T.Text
  , fpSequenceNumber :: Maybe Int
  , fpMeasureUnit :: Maybe MeasureUnit
  }
  deriving (Show, Generic)

instance FromJSON FoodPortion where
  parseJSON = recordParseJSON "fp"

data MeasureUnit = MeausureUnit
  { muId :: Maybe Int
  , muAbbreviation :: Maybe T.Text
  , muName :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON MeasureUnit where
  parseJSON = recordParseJSON "mu"

data InputFoodFoundation = InputFoodFoundation
  { iffId :: Maybe Int
  , iffFoodDescription :: Maybe T.Text
  , iffInputFood :: Maybe SampleFoodItem
  }
  deriving (Show, Generic)

instance FromJSON InputFoodFoundation where
  parseJSON = recordParseJSON "iff"

-- TODO missing foodGroup, foodAttributeTypes, totalRefuse
data SampleFoodItem = SampleFoodItem
  { sfiFdcId :: Int
  , sfiDataType :: T.Text
  , sfiDescription :: T.Text
  , sfiFoodClass :: Maybe T.Text
  , sfiPublicationDate :: Maybe T.Text
  , sfiFoodAttributes :: Maybe [T.Text]
  }
  deriving (Show, Generic)

instance FromJSON SampleFoodItem where
  parseJSON = recordParseJSON "sfi"

data NutrientConversionFactor = NutrientConversionFactor
  { ncfType :: Maybe T.Text
  , ncfValue :: Maybe Double
  }
  deriving (Show, Generic)

instance FromJSON NutrientConversionFactor where
  parseJSON = recordParseJSON "ncf"

-- NOTE all of these are maybe (unlike what the doc says)
data FoodNutrient = FoodNutrient
  { fnId :: Maybe Int
  , fnAmount :: Maybe Double
  , fnDataPoints :: Maybe Integer
  , fnMin :: Maybe Double
  , fnMax :: Maybe Double
  , fnMedian :: Maybe Double
  , fnType :: Maybe T.Text
  , fnNutrient :: Maybe Nutrient
  , -- NOTE not documented
    fnMinYearAcquired :: Maybe Int
  , -- NOTE not documented
    fnLoq :: Maybe Double
  , fnFoodNutrientDerivation :: Maybe NutrientDerivation
  , -- NOTE this is not documented as an array
    fnNutrientAnalysisDetails :: Maybe [NutrientAnalysisDetails]
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrient where
  parseJSON = recordParseJSON "fn"

data NutrientDerivation = NutrientDerivation
  { ndId :: Maybe Int
  , ndCode :: Maybe T.Text
  , ndDescription :: Maybe T.Text
  , ndFoodNutrientSource :: Maybe FoodNutrientSource
  }
  deriving (Show, Generic)

instance FromJSON NutrientDerivation where
  parseJSON = recordParseJSON "nd"

data Nutrient = Nutrient
  { nId :: Maybe Int
  , nNumber :: Maybe T.Text
  , nName :: Maybe T.Text
  , nRank :: Maybe Int
  , nUnitName :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON Nutrient where
  parseJSON = recordParseJSON "n"

data FoodNutrientDerivation = FoodNutrientDerivation
  { fndId :: Maybe Int
  , fndCode :: Maybe T.Text
  , fndDescription :: Maybe T.Text
  , fndFoodNutientSource :: Maybe FoodNutrientSource
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrientDerivation where
  parseJSON = recordParseJSON "fnd"

data FoodNutrientSource = FoodNutrientSource
  { fnsId :: Maybe Int
  , fnsCode :: Maybe T.Text
  , fnsDescription :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrientSource where
  parseJSON = recordParseJSON "fns"

data NutrientAnalysisDetails = NutrientAnalysisDetails
  { nadSubSampleId :: Maybe Int
  , nadAmount :: Maybe Double
  , nadNutrientId :: Maybe Int
  , nadLabMethodDescription :: Maybe T.Text
  , nadLabMethodOriginalDescription :: Maybe T.Text
  , nadLabMethodLink :: Maybe T.Text
  , nadLabMethodTechnique :: Maybe T.Text
  , -- NOTE not documented
    nadLoq :: Maybe Double
  , nadNutrientAcquisitionDetails :: Maybe [NutrientAcquisitionDetails]
  }
  deriving (Show, Generic)

instance FromJSON NutrientAnalysisDetails where
  parseJSON = recordParseJSON "nad"

data NutrientAcquisitionDetails = NutrientAcquisitionDetails
  { ncdSampleUnitId :: Maybe Int
  , ncdPurchaseDate :: Maybe T.Text
  , ncdStoreCity :: Maybe T.Text
  , ncdStoreState :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON NutrientAcquisitionDetails where
  parseJSON = recordParseJSON "ncd"

data FoodUpdateLog = FoodUpdateLog
  { fulFdcId :: Maybe Int
  , fulAvailableDate :: Maybe T.Text
  , fulBrandOwner :: Maybe T.Text
  , fulDataSource :: Maybe T.Text
  , fulDataType :: Maybe T.Text
  , fulDescription :: Maybe T.Text
  , fulFoodClass :: Maybe T.Text
  , fulGtinUpc :: Maybe T.Text
  , fulHouseholdServingFullText :: Maybe T.Text
  , fulIngredients :: Maybe T.Text
  , fulModifiedDate :: Maybe T.Text
  , fulPublicationDate :: Maybe T.Text
  , fulServingSize :: Maybe Double
  , fulServingSizeUnit :: Maybe T.Text
  , fulBrandedFoodCategory :: Maybe T.Text
  , fulChanges :: Maybe T.Text
  , fulFoodAttributes :: [FoodAttribute]
  }
  deriving (Show, Generic)

instance FromJSON FoodUpdateLog where
  parseJSON = recordParseJSON "ful"

data FoodAttribute = FoodAttribute
  { faID :: Maybe Int
  , faSequenceNumber :: Maybe Int
  , faValue :: Maybe T.Text
  , faFoodAttributeType :: FoodAttributeType
  }
  deriving (Show, Generic)

instance FromJSON FoodAttribute where
  parseJSON = recordParseJSON "fa"

data FoodAttributeType = FoodAttributeType
  { fatID :: Maybe Int
  , fatName :: Maybe T.Text
  , fatDescription :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON FoodAttributeType where
  parseJSON = recordParseJSON "fat"

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

recordParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
recordParseJSON s = genericParseJSON (recordOptions s)
