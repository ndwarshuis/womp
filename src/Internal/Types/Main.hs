{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Csv as C
import Data.Monoid
import Data.Monoid.Generic
import Data.Scientific
import GHC.Generics
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Map as M
import RIO.NonEmpty.Partial (fromList)
import qualified RIO.Text as T

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
      go :: T.Text -> Parser FoodItem
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
  , bfiBrandOwner :: T.Text
  , bfiDataSource :: Maybe T.Text
  , -- , bfiGtinUpc :: Maybe T.Text
    -- , bfiHouseholdServingFullText :: Maybe T.Text
    bfiIngredients :: Maybe T.Text
  , -- , bfiModifiedDate :: Maybe T.Text
    bfiServingSize :: Maybe Int
  , -- , bfiFormat :: Maybe Double
    bfiServingSizeUnit :: Maybe T.Text
    -- , bfiPreparationStateCode :: Maybe T.Text
    -- , bfiBrandedFoodCategory :: Maybe T.Text
    -- , bfiTradeChannel :: [T.Text]
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
      <*> v .: "brandOwner"
      <*> v .: "dataSource"
      <*> v .: "ingredients"
      <*> v .: "servingSize"
      <*> v .: "servingSizeUnit"

parseFoodRequiredMeta :: Object -> Parser FoodRequiredMeta
parseFoodRequiredMeta v =
  FoodRequiredMeta
    <$> v .: "fdcId"
    <*> v .: "description"

parseFoodCommon :: Object -> Parser FoodCommon
parseFoodCommon v =
  FoodCommon
    <$> v .:? "foodClass"
    <*> v .:? "publicationDate"
    <*> (v .:? "foodNutrients" .!= [])

data FoundationFoodItem = FoundationFoodItem
  { ffiMeta :: FoodRequiredMeta
  , ffiCommon :: FoundationLegacyCommon
  , ffiFootNote :: Maybe T.Text
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
  { frmId :: Int
  , frmDescription :: T.Text
  }
  deriving (Show)

data FoodCommon = FoodCommon
  { fcFoodClass :: Maybe T.Text
  , fcPublicationDate :: Maybe T.Text
  , fcFoodNutrients :: [FoodNutrient]
  }
  deriving (Show)

data FoundationLegacyCommon = FoundationLegacyCommon
  { flcCommon :: FoodCommon
  , flcIsHistoricalReference :: Maybe Bool
  , flcNdbNumber :: Maybe Int
  , flcScientificName :: Maybe T.Text
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

data ProteinConversion = ProteinConversion
  { pcFactor :: Scientific
  , pcAssumed :: Bool
  }
  deriving (Show)

parseFoundationLegacyCommon :: Object -> Parser FoundationLegacyCommon
parseFoundationLegacyCommon v = do
  ncf <- v .: "nutrientConversionFactors"
  c <- firstM caloreConv ncf
  p <- firstM proteinConv ncf
  FoundationLegacyCommon
    <$> parseFoodCommon v
    <*> v .:? "isHistoricalReference"
    <*> v .:? "ndbNumber"
    <*> v .:? "scientificName"
    <*> v .:? "foodCategory"
    <*> pure (fromMaybe defCalorie c)
    <*> pure (fromMaybe defProtein p)
  where
    caloreConv t = case (t :: T.Text) of
      ".CalorieConversionFactor" -> do
        f <- v .: "fatValue"
        p <- v .: "proteinValue"
        c <- v .: "carbohydrateValue"
        return $ Just $ CalorieConversion f p c False
      _ -> return Nothing
    proteinConv t = case (t :: T.Text) of
      ".ProteinConversionFactor" -> do
        p <- v .: "value"
        return $ Just $ ProteinConversion p False
      _ -> return Nothing
    defCalorie = CalorieConversion {ccFat = 9, ccProtein = 4, ccCarbs = 4, ccAssumed = True}
    defProtein = ProteinConversion 6.25 True

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
  , fcCode :: Maybe T.Text
  , fcDescription :: Maybe T.Text
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
  , nadAmount :: Maybe Scientific
  , -- , nadNutrientId :: Maybe Int
    nadLabMethodDescription :: Maybe T.Text
  , nadLabMethodOriginalDescription :: Maybe T.Text
  , -- , nadLabMethodLink :: Maybe T.Text
    nadLabMethodTechnique :: Maybe T.Text
    -- , nadNutrientAcquisitionDetails :: Maybe [NutrientAcquisitionDetails]
  }
  deriving (Show, Generic)

instance FromJSON NutrientAnalysisDetails where
  parseJSON = recordParseJSON "nad"

-- data NutrientAcquisitionDetails = NutrientAcquisitionDetails
--   { ncdSampleUnitId :: Maybe Int
--   , ncdPurchaseDate :: Maybe T.Text
--   , ncdStoreCity :: Maybe T.Text
--   , ncdStoreState :: Maybe T.Text
--   }
--   deriving (Show, Generic)

-- instance FromJSON NutrientAcquisitionDetails where
--   parseJSON = recordParseJSON "ncd"

data FoodUpdateLog = FoodUpdateLog
  { fulFdcId :: Maybe Int
  , fulAvailableDate :: Maybe T.Text
  , fulBrandOwner :: Maybe T.Text
  , fulDataSource :: Maybe T.Text
  , fulDescription :: Maybe T.Text
  , fulFoodClass :: Maybe T.Text
  , fulGtinUpc :: Maybe T.Text
  , fulHouseholdServingFullText :: Maybe T.Text
  , fulIngredients :: Maybe T.Text
  , fulModifiedDate :: Maybe T.Text
  , fulPublicationDate :: Maybe T.Text
  , fulServingSize :: Maybe Scientific
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

-- data RowNutrient = RowNutrient
--   { rnId :: Int
--   , rnMealName :: T.Text
--   , rnDesc :: T.Text
--   , rnNutrientName :: Maybe T.Text
--   , rnNutrientId :: Maybe Int
--   , rnDerivation :: Maybe T.Text
--   , rnAmount :: Maybe Scientific
--   , rnUnit :: Maybe T.Text
--   }
--   deriving (Generic, Show)

data RowNutrient = RowNutrient
  { rnNutrientId :: Int
  , rnNutrientName :: T.Text
  , rnAmount :: Scientific
  , rnUnit :: Unit
  }
  deriving (Generic, Show)

data RowSum = RowSum
  { rsNutrientName :: T.Text
  , rsNutrientId :: Int
  , rsAmount :: Scientific
  , rsUnit :: T.Text
  }
  deriving (Generic, Show)

instance C.ToRecord RowNutrient

instance C.ToRecord RowSum

data Dimensional = Dimensional
  { dimValue :: Scientific
  , dimUnit :: Unit
  }
  deriving (Show, Eq)

data UnitName
  = Gram
  | Calorie
  | Joule
  | IU
  deriving (Show, Eq)

data Unit = Unit
  { unitBase :: Prefix
  , unitName :: UnitName
  }
  deriving (Show, Eq)

tunit :: Unit -> Text
tunit Unit {unitName, unitBase} = T.append prefix unit
  where
    unit = case unitName of
      Calorie -> "cal"
      Joule -> "J"
      Gram -> "g"
      IU -> "IU"
    prefix = case unitBase of
      Nano -> "n"
      Micro -> "µ"
      Milli -> "m"
      Centi -> "c"
      Deci -> "d"
      Unity -> ""
      Deca -> "da"
      Hecto -> "h"
      Kilo -> "k"
      Mega -> "M"
      Giga -> "G"

instance C.ToField Unit where
  toField = encodeUtf8 . tunit

-- the good old heterogeneous list trick :)
data Meas = forall n. Measurable n => Meas n

instance Show Meas where
  show :: Meas -> String
  show (Meas x) = show $ toMeasuredInfo x

data Disp = forall n. Displayable n => Disp n

data MeasDisp = forall n. (Displayable n, Measurable n) => MeasDisp n

instance Show Disp where
  show :: Disp -> String
  show (Disp x) = show $ toDisplayInfo x

data NutrientValue_ a = NutrientValue
  { nvValue :: a
  , nvMembers :: NonEmpty Disp
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (NutrientValue_ a)

class Measurable a where
  toMeasuredInfo :: a -> (Int, T.Text)

  toId :: a -> Int
  toId = fst . toMeasuredInfo

  toName :: a -> T.Text
  toName = snd . toMeasuredInfo

class Displayable a where
  toDisplayInfo :: a -> (T.Text, Unit)

  toDName :: a -> T.Text
  toDName = fst . toDisplayInfo

  toDUnit :: a -> Unit
  toDUnit = snd . toDisplayInfo

data Proximates = Water | Protein | Lipid | Ash
  deriving (Show)

-- instance Measurable MeasuredProximates where
--   toMeasuredInfo :: MeasuredProximates -> (Int, T.Text)
--   toMeasuredInfo n = case n of
--     Water -> (1051, tshow n)
--     Protein -> (1003, tshow n)
--     Lipid -> (1004, tshow n)
--     Ash -> (1007, tshow n)

-- instance Displayable MeasuredProximates where
--   toDisplayInfo :: MeasuredProximates -> (T.Text, Unit)
--   toDisplayInfo n = (toName n, Unit Unity Gram)

-- data AppNutrient
--   = Starch
--   | TotalSugar
--   | TotalFat
--   deriving (Show)

data Lipids
  = FattyAcids
  | Cholesterol
  deriving (Show)

-- \| Phytosterols

data FattyAcids
  = SaturatedFattyAcids
  | MonounsaturatedFattyAcids
  | PolyunsaturatedFattyAcids
  | TransFattyAcids
  deriving (Show)

data Carbohydrates
  = Fiber
  | BetaGlucan
  | Starch
  deriving (Show)

data Fiber
  = FiberBySolubility
  | FiberByWeight
  deriving (Show)

data FiberBySolubility
  = SolubleFiber
  | InsolubleFiber
  deriving (Show, Enum, Bounded)

data FiberByWeight
  = HighMWFiber
  | LowMWFiber
  deriving (Show, Enum, Bounded)

data Sugars
  = SimpleSugars
  | OligoSaccharides
  deriving (Show)

data SimpleSugar
  = Sucrose
  | Glucose
  | Fructose
  | Lactose
  | Maltose
  | Galactose
  deriving (Show)

data OligoSaccharides
  = Raffinose
  | Stachyose
  | Verbascose
  deriving (Show)

-- data TotalFiber1992
--   = SolubleFiber
--   | InsolubleFiber
--   deriving (Show)

-- data TotalFiber2011
--   = HighMWFiber
--   | LowMWFiber
--   deriving (Show)

-- data MeasuredNutrient
--   = Nitrogen
--   deriving (Show)

-- data CalcNutrient
--   = Energy
--   | CarbDiff
--   | CarbSum
--   deriving (Show)

data AminoAcid
  = Tryptophan
  | Threonine
  | Isoleucine
  | Leucine
  | Lysine
  | Methionine
  | Cystine
  | Phenylalanine
  | Tyrosine
  | Valine
  | Arginine
  | Histidine
  | Alanine
  | AsparticAcid
  | GlutamicAcid
  | Glycine
  | Proline
  | Serine
  | Hydroxyproline
  | Asparagine
  | Cysteine
  | Glutamine
  | Taurine
  deriving (Show, Enum, Bounded)

data Mineral
  = Calcium
  | Iron
  | Magnesium
  | Phosphorus
  | Potassium
  | Sodium
  | Zinc
  | Copper
  | Iodine
  | Manganese
  | Molybdenum
  | Selenium
  deriving (Show, Enum, Bounded)

instance Measurable Carbohydrates where
  toMeasuredInfo :: Carbohydrates -> (Int, T.Text)
  toMeasuredInfo n = case n of
    Fiber -> (1079, "Soluble/Insoluble Fiber")
    BetaGlucan -> (2058, "Beta Glucans")
    Starch -> (1009, tshow n)

instance Measurable Mineral where
  toMeasuredInfo :: Mineral -> (Int, T.Text)
  toMeasuredInfo n = (,tshow n) $ case n of
    Calcium -> 1087
    Iron -> 1089
    Magnesium -> 1090
    Phosphorus -> 1091
    Potassium -> 1092
    Sodium -> 1093
    Zinc -> 1095
    Copper -> 1098
    Iodine -> 1100
    Manganese -> 1101
    Molybdenum -> 1102
    Selenium -> 1103

instance Measurable Fiber where
  toMeasuredInfo :: Fiber -> (Int, T.Text)
  toMeasuredInfo n = case n of
    FiberBySolubility -> (1079, "Soluble/Insoluble Fiber")
    FiberByWeight -> (2033, "High/Low Molecular Weight Fiber")

instance Measurable FiberBySolubility where
  toMeasuredInfo :: FiberBySolubility -> (Int, T.Text)
  toMeasuredInfo n = case n of
    SolubleFiber -> (1082, "Soluble Fiber")
    InsolubleFiber -> (1084, "Insoluble Fiber")

instance Measurable FiberByWeight where
  toMeasuredInfo :: FiberByWeight -> (Int, T.Text)
  toMeasuredInfo n = case n of
    HighMWFiber -> (2038, "High Molecular Weight Fiber")
    LowMWFiber -> (2065, "Low Molecular Weight Fiber")

-- Sugars -> (1063, tshow n)

-- instance Measurable AppNutrient where
--   toMeasuredInfo :: AppNutrient -> (Int, T.Text)
--   toMeasuredInfo n = case n of
--     Starch -> (1009, tshow n)
--     TotalSugar -> (1063, "Total Sugar")
--     Sucrose -> (1010, tshow n)
--     Glucose -> (1011, tshow n)
--     Fructose -> (1012, tshow n)
--     Lactose -> (1013, tshow n)
--     Maltose -> (1014, tshow n)
--     Galactose -> (1075, tshow n)
--     Raffinose -> (1076, tshow n)
--     Stachyose -> (1077, tshow n)
--     Verbascose -> (2063, tshow n)
--     TotalFiber1992 -> (1079, "Soluble/Insoluble Fiber")
--     SolubleFiber -> (1082, "Soluble Fiber")
--     InsolubleFiber -> (1084, "Insoluble Fiber")
--     TotalFiber2011 -> (2033, "High/Low Molecular Weight Fiber")
--     HighMWFiber -> (2038, "High Molecular Weight Fiber")
--     LowMWFiber -> (2065, "Low Molecular Weight Fiber")
--     BetaGlucan -> (2058, "Beta Glucans")
--     TotalFat -> (1004, "Total Fat")
--     Calcium -> (1087, tshow n)
--     Iron -> (1089, tshow n)
--     Magnesium -> (1090, tshow n)
--     Phosphorus -> (1091, tshow n)
--     Potassium -> (1092, tshow n)
--     Sodium -> (1093, tshow n)
--     Zinc -> (1095, tshow n)
--     Copper -> (1098, tshow n)
--     Iodine -> (1100, tshow n)
--     Manganese -> (1101, tshow n)
--     Molybdenum -> (1102, tshow n)
--     Selenium -> (1103, tshow n)

instance Measurable AminoAcid where
  toMeasuredInfo :: AminoAcid -> (Int, T.Text)
  toMeasuredInfo n = case n of
    Tryptophan -> (1210, tshow n)
    Threonine -> (1211, tshow n)
    Isoleucine -> (1212, tshow n)
    Leucine -> (1213, tshow n)
    Lysine -> (1214, tshow n)
    Methionine -> (1215, tshow n)
    Cystine -> (1216, tshow n)
    Phenylalanine -> (1217, tshow n)
    Tyrosine -> (1218, tshow n)
    Valine -> (1219, tshow n)
    Arginine -> (1220, tshow n)
    Histidine -> (1221, tshow n)
    Alanine -> (1222, tshow n)
    AsparticAcid -> (1223, "Aspartic Acid")
    GlutamicAcid -> (1224, "Glutamic Acid")
    Glycine -> (1225, tshow n)
    Proline -> (1226, tshow n)
    Serine -> (1227, tshow n)
    Hydroxyproline -> (1228, tshow n)
    Asparagine -> (1231, tshow n)
    Cysteine -> (1232, tshow n)
    Glutamine -> (1233, tshow n)
    Taurine -> (1234, tshow n)

-- instance Measurable MeasuredNutrient where
--   toMeasuredInfo :: MeasuredNutrient -> (Int, T.Text)
--   toMeasuredInfo Nitrogen = (1002, tshow Nitrogen)

-- instance Displayable CalcNutrient where
--   toDisplayInfo :: CalcNutrient -> (T.Text, Unit)
--   toDisplayInfo n = case n of
--     Energy -> (tshow n, Unit Kilo Calorie)
--     CarbDiff -> ("Carbohydrates (by difference)", Unit Unity Gram)
--     CarbSum -> ("Carbohydrates (by summation)", Unit Unity Gram)

-- instance Displayable AppNutrient where
--   toDisplayInfo :: AppNutrient -> (T.Text, Unit)
--   toDisplayInfo n = (toName n,) $ case n of
--     Starch -> g
--     TotalSugar -> g
--     Sucrose -> g
--     Glucose -> g
--     Fructose -> g
--     Lactose -> g
--     Maltose -> g
--     Galactose -> g
--     Raffinose -> g
--     Stachyose -> g
--     Verbascose -> g
--     TotalFiber1992 -> g
--     SolubleFiber -> g
--     InsolubleFiber -> g
--     TotalFiber2011 -> g
--     HighMWFiber -> g
--     LowMWFiber -> g
--     BetaGlucan -> g
--     TotalFat -> g
--     Calcium -> mg
--     Iron -> mg
--     Magnesium -> mg
--     Phosphorus -> mg
--     Potassium -> mg
--     Sodium -> mg
--     Zinc -> mg
--     Copper -> mg
--     Iodine -> mg
--     Manganese -> mg
--     Molybdenum -> mg
--     Selenium -> mg
--     where
--       g = Unit Unity Gram
--       mg = Unit Milli Gram

-- data NutrientHierarchy
--   = forall n.
--     Displayable n =>
--     DBranch n (NonEmpty NutrientHierarchy) NutrientHierarchy
--   | forall n.
--     (Displayable n, Measurable n) =>
--     MBranch n (NonEmpty NutrientHierarchy)
--   | forall n. (Displayable n, Measurable n) => MLeaf n
--   | forall n. Displayable n => DLeaf n

-- data NutrientHierarchy a
--   = forall b. DBranch a (Node b)
--   | forall b. MBranch a (NonEmpty (NutrientHierarchy b))
--   | MLeaf a
--   | DLeaf T.Text

data NutTree a = NutTree
  { ntFractions :: Branches a
  , ntPrefix :: Prefix
  , ntUnmeasuredHeader :: T.Text
  , ntUnmeasuredTree :: Maybe (Aggregation MNode)
  }

enumToNonEmpty :: (Bounded a, Enum a) => NonEmpty a
enumToNonEmpty = fromList [minBound ..]

toLeaves :: (Bounded a, Enum a) => Branches a
toLeaves = (\x -> Single (Right x, Nothing)) <$> enumToNonEmpty

data MNode = forall a. Measurable a => MNode (NutTree a)

type Branches a = NonEmpty (Aggregation (Either T.Text a, Maybe MNode))

nutHierarchy :: NutTree Proximates
nutHierarchy =
  NutTree
    { ntFractions =
        Single (Right Water, Nothing)
          :| [ Single (Right Lipid, Just undefined)
             , Single
                ( Right Protein
                , Just $
                    MNode $
                      NutTree
                        { ntFractions = toLeaves :: Branches AminoAcid
                        , ntPrefix = Milli
                        , ntUnmeasuredHeader = "Other Protein Mass"
                        , ntUnmeasuredTree = Nothing
                        }
                )
             , Single
                ( Right Ash
                , Just $
                    MNode $
                      NutTree
                        { ntFractions = toLeaves :: Branches Mineral
                        , ntPrefix = Milli
                        , ntUnmeasuredHeader = "Other Inorganics"
                        , ntUnmeasuredTree = Nothing
                        }
                )
             ]
    , ntPrefix = Unity
    , ntUnmeasuredHeader = "Carbohydrates (by difference)"
    , ntUnmeasuredTree =
        Just $
          Single $
            MNode $
              NutTree
                { ntFractions =
                    Single (Right Starch, Nothing)
                      :| [ Single (Right BetaGlucan, Nothing)
                         , Priority
                            ( ( Right FiberBySolubility
                              , Just $
                                  MNode $
                                    NutTree
                                      { ntFractions =
                                          Single (Right SolubleFiber, Nothing)
                                            :| [ Single (Right InsolubleFiber, Nothing)
                                               ]
                                      , ntPrefix = Unity
                                      , ntUnmeasuredHeader = "Other fiber"
                                      , ntUnmeasuredTree = Nothing
                                      }
                              )
                                :| [
                                     ( Right FiberByWeight
                                     , Just $
                                        MNode $
                                          NutTree
                                            { ntFractions =
                                                Single (Right HighMWFiber, Nothing)
                                                  :| [ Single (Right LowMWFiber, Nothing)
                                                     ]
                                            , ntPrefix = Unity
                                            , ntUnmeasuredHeader = "Other fiber"
                                            , ntUnmeasuredTree = Nothing
                                            }
                                     )
                                   ]
                            )
                         , Single (Left "Total Sugars", Just undefined)
                         ]
                , ntPrefix = Unity
                , ntUnmeasuredHeader = "Other carbohydrates"
                , ntUnmeasuredTree = Nothing
                }
    }

data Aggregation a = Single a | Priority (NonEmpty a)

data MTree a = MTree
  { mtMass :: Scientific
  , mtKnown :: M.Map a (MTree a)
  , mtUnknown :: M.Map a (UTree a)
  }

data UTree a = UTree
  { utKnown :: M.Map a (MTree a)
  , utUnknown :: M.Map a (UTree a)
  }

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
  deriving (Show, Eq)

instance Exception AppException

newtype AppException = AppException [AppError]
  deriving (Show, Semigroup) via [AppError]

type MonadAppError = MonadError AppException

type AppExcept = AppExceptT Identity

type AppExceptT = ExceptT AppException

data AppError
  = DatePatternError !Natural !Natural !(Maybe Natural) !PatternSuberr
  | UnitMatchError !Dimensional !Dimensional
  | UnitParseError !T.Text
  | DaySpanError !Int
  | -- TODO store FDC id/name and nutrient name here too
    NutrientError !Meas
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)
