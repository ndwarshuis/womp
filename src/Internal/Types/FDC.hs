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
  , ffiFoodPortion :: Maybe [FoodPortion]
  , ffiInputFoods :: Maybe [InputFoodFoundation]
  , ffiNutrientConversionFactors :: Maybe [NutrientConversionFactor]
  }
  deriving (Show, Generic)

instance FromJSON FoundationFoodItem where
  parseJSON = recordParseJSON "ffi"

data FoodCategory = FoodCategory
  { fcID :: Maybe Int
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
  deriving (Show, Generic, FromJSON)

data InputFoodFoundation = InputFoodFoundation
  { iffID :: Maybe Int
  , iffDescription :: Maybe T.Text
  , iffInputFood :: Maybe SampleFoodItem
  }
  deriving (Show, Generic)

instance FromJSON InputFoodFoundation where
  parseJSON = recordParseJSON "iff"

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
  deriving (Show, Generic, FromJSON)

data FoodNutrient = FoodNutrient
  { fnNumber :: Maybe Int
  , fnName :: Maybe T.Text
  , fnAmount :: Maybe Double
  , fnUnitName :: Maybe T.Text
  , fnDerivationCode :: Maybe T.Text
  , fnDerivationDescription :: Maybe T.Text
  }
  deriving (Show, Generic)

instance FromJSON FoodNutrient where
  parseJSON = recordParseJSON "fn"

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
    }

recordParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
recordParseJSON s = genericParseJSON (recordOptions s)
