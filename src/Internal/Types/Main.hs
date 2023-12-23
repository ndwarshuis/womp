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

type FoodTree = FoodTree_ NutrientValue

-- TOOO this doesn't make much sense if we are going to be mashing trees
-- together with monoid math, unless I turn the name and id fields into lists to
-- reflect the fact that multiple ingredients will be included
data FoodTree_ a = FoodTree
  { ftName :: T.Text
  , ftId :: Int
  , ftCalculated :: CalculatedTree_ a
  }
  deriving (Show, Generic, Functor)

type CalculatedTree = CalculatedTree_ NutrientValue

data CalculatedTree_ a = CalculatedTree
  { fctProximates :: ProximateTree_ a
  , fctEnergy :: a
  , fctCarbDiff :: a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (CalculatedTree_ a)

-- deriving (Monoid) via GenericMonoid (CalculatedTree_ a)

type ProximateTree = ProximateTree_ NutrientValue

data ProximateTree_ a = ProximateTree
  { ptProtein :: Proteins_ a
  , ptCarbohydrates_ :: Carbohydrates_ a
  , ptLipids :: Lipids_ a
  , ptAsh :: AshFraction_ a
  , ptWater :: a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (ProximateTree_ a)

-- deriving (Monoid) via GenericMonoid (ProximateTree_ a)

type Proteins = Proteins_ NutrientValue

data Proteins_ a = Proteins
  { pTotal :: a
  -- TODO add amino acid composition to this
  -- , pAminoAcids :: TODO
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Proteins_ a)

-- deriving (Monoid) via GenericMonoid (Proteins_ a)

-- data AminoAcids = AminoAcids
--   { aaTryptophan :: Maybe Scientific
--   , aaThreonine :: Maybe Scientific
--   , aaIsoleucine :: Maybe Scientific
--   , aaLeucine :: Maybe Scientific
--   , aaLysine :: Maybe Scientific
--   , aaMethionine :: Maybe Scientific
--   , aaCystine :: Maybe Scientific
--   , aaPhenylalanine :: Maybe Scientific
--   , aaTyrosine :: Maybe Scientific
--   , aaValine :: Maybe Scientific
--   , aaArginine :: Maybe Scientific
--   , aaHistidine :: Maybe Scientific
--   , aaAlanine :: Maybe Scientific
--   , aaAsparticAcid :: Maybe Scientific
--   , aaGlutamicAcid :: Maybe Scientific
--   , aaGlycine :: Maybe Scientific
--   , aaProline :: Maybe Scientific
--   , aaSerine :: Maybe Scientific
--   , aaHydroxyproline :: Maybe Scientific
--   , aaAsparagine :: Maybe Scientific
--   , aaCysteine :: Maybe Scientific
--   , aaGlutamine :: Maybe Scientific
--   , aaTaurine :: Maybe Scientific
--   }
--   deriving (Show)

-- data ProteinMethod = Dumas | Kjeldahl
--   deriving (Show)

type Carbohydrates = Carbohydrates_ NutrientValue

data Carbohydrates_ a = Carbohydrates
  { chSugars_ :: Sugars_ a
  , chOligos :: OligoSaccharides_ a
  , chFiber :: Fiber_ a
  , chStarch :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Carbohydrates_ a)

-- deriving (Monoid) via GenericMonoid (Carbohydrates_ a)

type Sugars = Sugars_ NutrientValue

data Sugars_ a = Sugars
  { sTotal :: Maybe a
  , sSucrose :: Maybe a
  , sGlucose :: Maybe a
  , sFructose :: Maybe a
  , sLactose :: Maybe a
  , sMaltose :: Maybe a
  , sGalactose :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Sugars_ a)

-- deriving (Monoid) via GenericMonoid (Sugars_ a)

type OligoSaccharides = OligoSaccharides_ NutrientValue

data OligoSaccharides_ a = OligoSaccharides
  { osRaffinose :: Maybe a
  , osStachyose :: Maybe a
  , osVerbascose :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (OligoSaccharides_ a)

-- deriving (Monoid) via GenericMonoid (OligoSaccharides_ a)

type Fiber = Fiber_ NutrientValue

data Fiber_ a = Fiber
  { fFractions1992 :: Maybe (FiberFractions1992_ a)
  , fFractions2011 :: Maybe (FiberFractions2011_ a)
  , -- technically a subset of soluble fiber
    fBetaGlucan :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Fiber_ a)

-- deriving (Monoid) via GenericMonoid (Fiber_ a)

type FiberFractions1992 = FiberFractions1992_ NutrientValue

data FiberFractions1992_ a = FiberFractions1992
  { ffTotal1992 :: a
  , ffSoluble :: Maybe a
  , ffInsoluble :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (FiberFractions1992_ a)

-- deriving (Monoid) via GenericMonoid (FiberFractions1992_ a)

type FiberFractions2011 = FiberFractions2011_ NutrientValue

data FiberFractions2011_ a = FiberFractions2011
  { ffTotal2011 :: a
  , ffHMW :: Maybe a
  , ffLMW :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (FiberFractions2011_ a)

-- deriving (Monoid) via GenericMonoid (FiberFractions2011_ a)

type Lipids = Lipids_ NutrientValue

-- TODO break out the subtypes into their individual components
data Lipids_ a = Lipids
  { lTotal :: a
  -- , lTFA :: Maybe Scientific
  -- , lSFA :: Maybe Scientific
  -- , lMUFA :: Maybe Scientific
  -- , lPUFA :: Maybe Scientific
  -- TODO add cholesterol
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Lipids_ a)

-- deriving (Monoid) via GenericMonoid (Lipids_ a)

-- data TotalLipds_ = TotalLipds_
--   { tlAmount :: Scientific
--   , tlMethod :: TotalLipidMethod
--   }
--   deriving (Show)

-- NOTE: all except GLC are used for total fat determination, all the subtypes
-- are determined via GLC
-- data TotalLipidMethod
--   = -- acid hydrolysis followed by extraction
--     AcidHydrolysis
--   | -- base hydrolysis followed by extraction
--     BaseHydrolysis
--   | -- soxhlet extraction or similar
--     Extraction
--   | -- gas-liquid chromatography
--     GLC
--   deriving (Show)

type AshFraction = AshFraction_ NutrientValue

data AshFraction_ a = AshFraction
  { aTotal :: a
  , aMinerals_ :: Minerals_ a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (AshFraction_ a)

-- deriving (Monoid) via GenericMonoid (Ash_ a)

type Minerals = Minerals_ NutrientValue

data Minerals_ a = Minerals
  { mSodium :: Maybe a
  , mMagnesium :: Maybe a
  , mPhosphorus :: Maybe a
  , mPotassium :: Maybe a
  , mCalcium :: Maybe a
  , mManganese :: Maybe a
  , mIron :: Maybe a
  , mCopper :: Maybe a
  , mZinc :: Maybe a
  , mSelenium :: Maybe a
  , mMolybdenum :: Maybe a
  , mIodine :: Maybe a
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (Minerals_ a)

-- deriving (Monoid) via GenericMonoid (Minerals_ a)

data FoodMeta = FoodMeta
  { fmId :: Int
  , fmName :: T.Text
  }
  deriving (Show)

type NutrientValue = NutrientValue_ (Sum Scientific)

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

-- TODO these can be categorized into the following groups
-- - direct or calculated and displayed (protein)
-- - direct and displayed (most of these)
-- - calculated and displayed (differential carbohydrates)
-- - direct and not displayed
--
-- anything directly parsed needs an ID (duh)
-- anything to be displayed needs a string and unit

data AppNutrient
  = Water
  | Protein
  | Starch
  | TotalSugar
  | Sucrose
  | Glucose
  | Fructose
  | Lactose
  | Maltose
  | Galactose
  | Raffinose
  | Stachyose
  | Verbascose
  | TotalFiber1992
  | SolublseFiber
  | InsolublseFiber
  | TotalFiber2011
  | HighMWFiber
  | LowMWFiber
  | BetaGlucan
  | TotalFat
  | Ash
  | Calcium
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
  deriving (Show)

data MeasuredNutrient
  = Nitrogen
  deriving (Show)

data CalcNutrient
  = Energy
  | CarbDiff
  | CarbSum
  deriving (Show)

instance Measurable MeasuredNutrient where
  toMeasuredInfo :: MeasuredNutrient -> (Int, T.Text)
  toMeasuredInfo Nitrogen = (1002, tshow Nitrogen)

instance Displayable CalcNutrient where
  toDisplayInfo :: CalcNutrient -> (T.Text, Unit)
  toDisplayInfo n = case n of
    Energy -> (tshow n, Unit Kilo Calorie)
    CarbDiff -> ("Carbohydrates (by difference)", Unit Unity Gram)
    CarbSum -> ("Carbohydrates (by summation)", Unit Unity Gram)

instance Measurable AppNutrient where
  toMeasuredInfo :: AppNutrient -> (Int, T.Text)
  toMeasuredInfo n = case n of
    Water -> (1051, tshow n)
    Protein -> (1003, tshow n)
    Starch -> (1009, tshow n)
    TotalSugar -> (1063, "Total Sugar")
    Sucrose -> (1010, tshow n)
    Glucose -> (1011, tshow n)
    Fructose -> (1012, tshow n)
    Lactose -> (1013, tshow n)
    Maltose -> (1014, tshow n)
    Galactose -> (1075, tshow n)
    Raffinose -> (1076, tshow n)
    Stachyose -> (1077, tshow n)
    Verbascose -> (2063, tshow n)
    TotalFiber1992 -> (1079, "Soluble/Insoluble Fiber")
    SolublseFiber -> (1082, "Soluble Fiber")
    InsolublseFiber -> (1084, "Insoluble Fiber")
    TotalFiber2011 -> (2033, "High/Low Molecular Weight Fiber")
    HighMWFiber -> (2038, "High Molecular Weight Fiber")
    LowMWFiber -> (2065, "Low Molecular Weight Fiber")
    BetaGlucan -> (2058, "Beta Glucans")
    TotalFat -> (1004, "Total Fat")
    Ash -> (1007, tshow n)
    Calcium -> (1087, tshow n)
    Iron -> (1089, tshow n)
    Magnesium -> (1090, tshow n)
    Phosphorus -> (1091, tshow n)
    Potassium -> (1092, tshow n)
    Sodium -> (1093, tshow n)
    Zinc -> (1095, tshow n)
    Copper -> (1098, tshow n)
    Iodine -> (1100, tshow n)
    Manganese -> (1101, tshow n)
    Molybdenum -> (1102, tshow n)
    Selenium -> (1103, tshow n)

instance Displayable AppNutrient where
  toDisplayInfo :: AppNutrient -> (T.Text, Unit)
  toDisplayInfo n = (toName n,) $ case n of
    Water -> g
    Protein -> g
    Starch -> g
    TotalSugar -> g
    Sucrose -> g
    Glucose -> g
    Fructose -> g
    Lactose -> g
    Maltose -> g
    Galactose -> g
    Raffinose -> g
    Stachyose -> g
    Verbascose -> g
    TotalFiber1992 -> g
    SolublseFiber -> g
    InsolublseFiber -> g
    TotalFiber2011 -> g
    HighMWFiber -> g
    LowMWFiber -> g
    BetaGlucan -> g
    TotalFat -> g
    Ash -> g
    Calcium -> mg
    Iron -> mg
    Magnesium -> mg
    Phosphorus -> mg
    Potassium -> mg
    Sodium -> mg
    Zinc -> mg
    Copper -> mg
    Iodine -> mg
    Manganese -> mg
    Molybdenum -> mg
    Selenium -> mg
    where
      g = Unit Unity Gram
      mg = Unit Milli Gram

data NutrientHierarchy
  = MBranch Meas NutrientHierarchy
  | -- nutrient with subcomponents and optionally a difference nutrient, head
    -- nutrient must be both measurable and displayable
    DBranch MeasDisp (NonEmpty NutrientHierarchy) (Maybe Disp)
  | -- nutrient with no subcomponents, must be both measurable and displayable
    Leaf MeasDisp

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
