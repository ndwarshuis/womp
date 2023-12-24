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
import RIO.State
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

type NutrientReader = MonadReader FoodMeta

type FoodState = [FoodNutrient]

type NutrientState = MonadState FoodState

data MeasuredNutrient
  = Direct DirectNutrient
  | Computed ComputedNutrient
  | Alternate AltNutrient
  deriving (Show, Eq, Ord)

-- TODO augment this to include alternatively calculated nutrients (like protein
-- and some MUFAs) which will required a monadic function that executes in a
-- reader env with whatever data we need
data DirectNutrient = DirectNutrient
  { mnId :: Int
  , mnName :: T.Text
  , mnDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

data AltNutrient = AltNutrient
  { anName :: T.Text
  , anDisplayPrefix :: Prefix
  , anChoices :: NonEmpty Int
  }
  deriving (Show, Eq, Ord)

data ComputedNutrient = ComputedNutrient
  { cnName :: T.Text
  , cnCompute :: FoodMeta -> FoodState -> Scientific
  }

-- TODO this is kinda hacky but there shouldn't be many of these to compare
-- so it likely won't matter much
instance Show ComputedNutrient where
  show ComputedNutrient {cnName} = "ComputedNutrient(" ++ T.unpack cnName ++ ")"

instance Eq ComputedNutrient where
  (==) a b = cnName a == cnName b

instance Ord ComputedNutrient where
  compare a b = compare (cnName a) (cnName b)

data SummedNutrient = SummedNutrient
  { snName :: T.Text
  , snDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

data FoodMeta = FoodMeta
  { fmDesc :: T.Text
  , fmId :: Int
  , fmNitrogenFactor :: Maybe Scientific
  }
  deriving (Show)

data NutrientValue_ a = NutrientValue
  { nvValue :: a
  , nvMembers :: NonEmpty FoodMeta
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (NutrientValue_ a)

-- | Proximate level
water :: MeasuredNutrient
water = Direct $ DirectNutrient 1051 "Water" Unity

-- TODO nitrogen = 1002
protein :: MeasuredNutrient
protein = Direct $ DirectNutrient 1003 "Protein" Unity

lipid :: MeasuredNutrient
lipid = Direct $ DirectNutrient 1004 "Lipids" Unity

ash :: MeasuredNutrient
ash = Direct $ DirectNutrient 1007 "Ash" Unity

carbDiff :: SummedNutrient
carbDiff = SummedNutrient "Carbohydrates (by difference)" Unity

-- | Lipid level
tfas :: MeasuredNutrient
tfas = Direct $ DirectNutrient 1257 "Trans Fatty Acids" Unity

mufas :: MeasuredNutrient
mufas = Direct $ DirectNutrient 1292 "Monounsaturated Fatty Acids" Unity

pufas :: MeasuredNutrient
pufas = Direct $ DirectNutrient 1293 "Polyunsaturated Fatty Acids" Unity

sfas :: MeasuredNutrient
sfas = Direct $ DirectNutrient 1258 "Saturated Fatty Acids" Unity

cholesterol :: MeasuredNutrient
cholesterol = Direct $ DirectNutrient 1253 "Cholesterol" Unity

phytosterols :: SummedNutrient
phytosterols = SummedNutrient "Phytosterols" Unity

-- this doesn't strictly seem like a phytosterol but is still included in the
-- same section along with the rest (whatever)
stigmastadiene :: MeasuredNutrient
stigmastadiene = Direct $ DirectNutrient 2052 "Stigmastadiene" Micro

stigmasterol :: MeasuredNutrient
stigmasterol = Direct $ DirectNutrient 1285 "Stigmasterol" Micro

campesterol :: MeasuredNutrient
campesterol = Direct $ DirectNutrient 1286 "Campesterol" Micro

brassicasterol :: MeasuredNutrient
brassicasterol = Direct $ DirectNutrient 1287 "Brassicasterol" Micro

betaSitosterol :: MeasuredNutrient
betaSitosterol = Direct $ DirectNutrient 1288 "BetaSitosterol" Micro

campestanol :: MeasuredNutrient
campestanol = Direct $ DirectNutrient 1289 "Campestanol" Micro

betaSitostanol :: MeasuredNutrient
betaSitostanol = Direct $ DirectNutrient 1294 "BetaSitostanol" Micro

delta_5_avenasterol :: MeasuredNutrient
delta_5_avenasterol = Direct $ DirectNutrient 1296 "Delta5Avenasterol" Micro

delta_7_stigmastenol :: MeasuredNutrient
delta_7_stigmastenol = Direct $ DirectNutrient 2052 "Delta7Stigmastenol" Micro

ergosterol :: MeasuredNutrient
ergosterol = Direct $ DirectNutrient 1284 "Ergosterol" Micro

ergosta_7_enol :: MeasuredNutrient
ergosta_7_enol = Direct $ DirectNutrient 2060 "Ergosta-7-enol" Micro

ergosta_7_22_dienol :: MeasuredNutrient
ergosta_7_22_dienol = Direct $ DirectNutrient 2061 "Ergosta-7,22-dienol" Micro

ergosta_5_7_dienol :: MeasuredNutrient
ergosta_5_7_dienol = Direct $ DirectNutrient 2062 "Ergosta-5,7-dienol" Micro

-- TODO not exactly sure what this means, but hopefully it means "not the above"
otherPhytosterols :: MeasuredNutrient
otherPhytosterols = Direct $ DirectNutrient 1298 "Other Phytosterols" Unity

sfa_4_0 :: MeasuredNutrient
sfa_4_0 = Direct $ DirectNutrient 1259 "SFA 4:0" Micro

sfa_5_0 :: MeasuredNutrient
sfa_5_0 = Direct $ DirectNutrient 2003 "SFA 5:0" Micro

sfa_6_0 :: MeasuredNutrient
sfa_6_0 = Direct $ DirectNutrient 1260 "SFA 6:0" Micro

sfa_7_0 :: MeasuredNutrient
sfa_7_0 = Direct $ DirectNutrient 2004 "SFA 7:0" Micro

sfa_8_0 :: MeasuredNutrient
sfa_8_0 = Direct $ DirectNutrient 1261 "SFA 8:0" Micro

sfa_9_0 :: MeasuredNutrient
sfa_9_0 = Direct $ DirectNutrient 2005 "SFA 9:0" Micro

sfa_10_0 :: MeasuredNutrient
sfa_10_0 = Direct $ DirectNutrient 1262 "SFA 10:0" Micro

sfa_11_0 :: MeasuredNutrient
sfa_11_0 = Direct $ DirectNutrient 1335 "SFA 11:0" Micro

sfa_12_0 :: MeasuredNutrient
sfa_12_0 = Direct $ DirectNutrient 1263 "SFA 12:0" Micro

sfa_14_0 :: MeasuredNutrient
sfa_14_0 = Direct $ DirectNutrient 1264 "SFA 14:0" Micro

sfa_15_0 :: MeasuredNutrient
sfa_15_0 = Direct $ DirectNutrient 1299 "SFA 15:0" Micro

sfa_16_0 :: MeasuredNutrient
sfa_16_0 = Direct $ DirectNutrient 1265 "SFA 16:0" Micro

sfa_17_0 :: MeasuredNutrient
sfa_17_0 = Direct $ DirectNutrient 1300 "SFA 17:0" Micro

sfa_18_0 :: MeasuredNutrient
sfa_18_0 = Direct $ DirectNutrient 1266 "SFA 18:0" Micro

sfa_20_0 :: MeasuredNutrient
sfa_20_0 = Direct $ DirectNutrient 1267 "SFA 20:0" Micro

sfa_21_0 :: MeasuredNutrient
sfa_21_0 = Direct $ DirectNutrient 2006 "SFA 21:0" Micro

sfa_22_0 :: MeasuredNutrient
sfa_22_0 = Direct $ DirectNutrient 1273 "SFA 22:0" Micro

sfa_23_0 :: MeasuredNutrient
sfa_23_0 = Direct $ DirectNutrient 2007 "SFA 23:0" Micro

sfa_24_0 :: MeasuredNutrient
sfa_24_0 = Direct $ DirectNutrient 1301 "SFA 24:0" Micro

mufa_12_1 :: MeasuredNutrient
mufa_12_1 = Direct $ DirectNutrient 2008 "MUFA 12:1" Micro

-- NOTE: it seems most of the MUFAs are not characterized further other than
-- number of carbons. In this case, several of the MUFAs are redundant (ie the
-- one's that end in c and the ones that don't). Rather than have multiple
-- levels for each, collapse them into single layer to keep my code less lame

-- TODO not sure how this one works, it is almost always analytical but
-- sometimes summed in which case I have no idea what the inputs are because
-- there are no other 14C MUFAs in the db)
mufa_14_1 :: MeasuredNutrient
mufa_14_1 = Direct $ DirectNutrient 2009 "MUFA 14:1" Micro

mufa_15_1 :: MeasuredNutrient
mufa_15_1 = Direct $ DirectNutrient 1333 "MUFA 15:1" Micro

mufa_16_1 :: MeasuredNutrient
mufa_16_1 = Direct $ DirectNutrient 1314 "MUFA 16:1" Micro

mufa_17_1 :: MeasuredNutrient
mufa_17_1 = Direct $ DirectNutrient 1323 "MUFA 17:1" Micro

mufa_18_1 :: MeasuredNutrient
mufa_18_1 = Alternate $ AltNutrient "MUFA 18:1" Micro $ 1315 :| [1268]

mufa_20_1 :: MeasuredNutrient
mufa_20_1 = Alternate $ AltNutrient "MUFA 20:1" Micro $ 2012 :| [1277]

mufa_22_1 :: MeasuredNutrient
mufa_22_1 = Alternate $ AltNutrient "MUFA 22:1" Micro $ 1317 :| [2012]

mufa_22_1_n9 :: MeasuredNutrient
mufa_22_1_n9 = Direct $ DirectNutrient 2014 "MUFA 22:1 ω-9 (Erucic Acid)" Micro

mufa_22_1_n11 :: MeasuredNutrient
mufa_22_1_n11 = Direct $ DirectNutrient 2015 "MUFA 22:1 ω-11" Micro

mufa_24_1_c :: MeasuredNutrient
mufa_24_1_c = Direct $ DirectNutrient 1312 "MUFA 24:1 c" Micro

tfa_14_1 :: MeasuredNutrient
tfa_14_1 = Direct $ DirectNutrient 1281 "TFA 14:1" Unity

tfa_16_1 :: MeasuredNutrient
tfa_16_1 = Direct $ DirectNutrient 1303 "TFA 16:1" Unity

tfa_17_1 :: MeasuredNutrient
tfa_17_1 = Direct $ DirectNutrient 2011 "TFA 17:1" Unity

tfa_18_1 :: MeasuredNutrient
tfa_18_1 = Direct $ DirectNutrient 1304 "TFA 18:1" Unity

tfa_18_2 :: MeasuredNutrient
tfa_18_2 = Direct $ DirectNutrient 1306 "TFA 18:2" Unity

tfa_18_3 :: MeasuredNutrient
tfa_18_3 = Direct $ DirectNutrient 2019 "TFA 18:3" Unity

tfa_20_1 :: MeasuredNutrient
tfa_20_1 = Direct $ DirectNutrient 2013 "TFA 20:1" Unity

tfa_22_1 :: MeasuredNutrient
tfa_22_1 = Direct $ DirectNutrient 1305 "TFA 22:1" Unity

pufa_18_2 :: MeasuredNutrient
pufa_18_2 = Direct $ DirectNutrient 1269 "PUFA 18:2" Unity

pufa_18_2_CLA :: MeasuredNutrient
pufa_18_2_CLA = Direct $ DirectNutrient 1311 "PUFA 18:2 (conjugated linoleic acids)" Unity

pufa_18_2_n6_cc :: MeasuredNutrient
pufa_18_2_n6_cc = Direct $ DirectNutrient 1316 "PUFA 18:2 ω-6 c,c (Linoleic Acid)" Unity

pufa_18_3 :: MeasuredNutrient
pufa_18_3 = Direct $ DirectNutrient 1270 "PUFA 18:3" Unity

-- at least I think this is what "i" means
pufa_18_3i :: MeasuredNutrient
pufa_18_3i = Direct $ DirectNutrient 1409 "PUFA 18:2 isomers" Unity

pufa_18_3_n6_ccc :: MeasuredNutrient
pufa_18_3_n6_ccc = Direct $ DirectNutrient 1321 "PUFA 18:3 ω-6 c,c,c (Gamma-linolenic Acid)" Unity

pufa_18_3_n3_ccc :: MeasuredNutrient
pufa_18_3_n3_ccc = Direct $ DirectNutrient 1404 "PUFA 18:3 ω-3 c,c,c (Alpha-linolenic Acid)" Unity

pufa_18_4 :: MeasuredNutrient
pufa_18_4 = Direct $ DirectNutrient 1276 "PUFA 18:4" Unity

pufa_20_2 :: SummedNutrient
pufa_20_2 = SummedNutrient "PUFA 20:2" Unity

pufa_20_2_n6_cc :: MeasuredNutrient
pufa_20_2_n6_cc = Direct $ DirectNutrient 1313 "PUFA 20:2_n6_cc" Unity

pufa_20_3 :: MeasuredNutrient
pufa_20_3 = Direct $ DirectNutrient 1325 "PUFA 20:3" Unity

pufa_20_3_n3 :: MeasuredNutrient
pufa_20_3_n3 = Direct $ DirectNutrient 1405 "PUFA 20:3 ω-3 c,c,c (Eicosatetraenoic Acid)" Unity

pufa_20_3_n6 :: MeasuredNutrient
pufa_20_3_n6 = Direct $ DirectNutrient 1406 "PUFA 20:3 ω-6 c,c,c (Dihomo-gamma-linolenic Acid)" Unity

pufa_20_3_n9 :: MeasuredNutrient
pufa_20_3_n9 = Direct $ DirectNutrient 1414 "PUFA 20:3 ω-9 c,c,c (Mead Acid)" Unity

pufa_20_4 :: MeasuredNutrient
pufa_20_4 = Direct $ DirectNutrient 1271 "PUFA 20:4" Unity

pufa_20_5 :: SummedNutrient
pufa_20_5 = SummedNutrient "PUFA 20:5" Unity

pufa_20_5_n3 :: MeasuredNutrient
pufa_20_5_n3 = Direct $ DirectNutrient 1278 "PUFA 20:5_n3" Unity

pufa_22_2 :: MeasuredNutrient
pufa_22_2 = Direct $ DirectNutrient 1334 "PUFA 22:2" Unity

pufa_22_3 :: MeasuredNutrient
pufa_22_3 = Direct $ DirectNutrient 2021 "PUFA 22:3" Unity

pufa_22_4 :: MeasuredNutrient
pufa_22_4 = Direct $ DirectNutrient 1411 "PUFA 22:4" Unity

pufa_22_5 :: SummedNutrient
pufa_22_5 = SummedNutrient "PUFA 22:5" Unity

pufa_22_5_n3 :: MeasuredNutrient
pufa_22_5_n3 = Direct $ DirectNutrient 1280 "PUFA 22:5 ω-3 c,c,c,c,c (Docosapentaenoic Acid)" Unity

pufa_22_6 :: SummedNutrient
pufa_22_6 = SummedNutrient "PUFA 22:5" Unity

pufa_22_6_n3 :: MeasuredNutrient
pufa_22_6_n3 = Direct $ DirectNutrient 1272 "PUFA 22:6 ω-3 c,c,c,c,c,c (Docosahexaenoic Acid)" Unity

-- | Carbohydrate level
fiber :: MeasuredNutrient
fiber = Direct $ DirectNutrient 1079 "Soluble/Insoluble Fiber" Unity

betaGlucan :: MeasuredNutrient
betaGlucan = Direct $ DirectNutrient 2058 "Beta Glucans" Unity

starch :: MeasuredNutrient
starch = Direct $ DirectNutrient 1009 "Starch" Unity

fiberBySolubility :: MeasuredNutrient
fiberBySolubility = Direct $ DirectNutrient 1079 "Soluble/Insoluble Fiber" Unity

fiberByWeight :: MeasuredNutrient
fiberByWeight = Direct $ DirectNutrient 2033 "High/Low Molecular Weight Fiber" Unity

highMWFiber :: MeasuredNutrient
highMWFiber = Direct $ DirectNutrient 2038 "High Molecular Weight Fiber" Unity

lowMWFiber :: MeasuredNutrient
lowMWFiber = Direct $ DirectNutrient 2065 "Low Molecular Weight Fiber" Unity

solubleFiber :: MeasuredNutrient
solubleFiber = Direct $ DirectNutrient 1082 "Soluble Fiber" Unity

insolubleFiber :: MeasuredNutrient
insolubleFiber = Direct $ DirectNutrient 1084 "Insoluble Fiber" Unity

-- | Sugar level
sucrose :: MeasuredNutrient
sucrose = Direct $ DirectNutrient 1010 "Sucrose" Unity

glucose :: MeasuredNutrient
glucose = Direct $ DirectNutrient 1011 "Glucose" Unity

fructose :: MeasuredNutrient
fructose = Direct $ DirectNutrient 1012 "Fructose" Unity

lactose :: MeasuredNutrient
lactose = Direct $ DirectNutrient 1013 "Lactose" Unity

maltose :: MeasuredNutrient
maltose = Direct $ DirectNutrient 1014 "maltose" Unity

galactose :: MeasuredNutrient
galactose = Direct $ DirectNutrient 1075 "Galactose" Unity

raffinose :: MeasuredNutrient
raffinose = Direct $ DirectNutrient 1076 "Raffinose" Unity

stachyose :: MeasuredNutrient
stachyose = Direct $ DirectNutrient 1077 "Stachyose" Unity

verbascose :: MeasuredNutrient
verbascose = Direct $ DirectNutrient 2063 "Verbascose" Unity

tryptophan :: MeasuredNutrient
tryptophan = Direct $ DirectNutrient 1210 "Tryptophan" Milli

threonine :: MeasuredNutrient
threonine = Direct $ DirectNutrient 1211 "Threonine" Milli

isoleucine :: MeasuredNutrient
isoleucine = Direct $ DirectNutrient 1212 "Isoleucine" Milli

leucine :: MeasuredNutrient
leucine = Direct $ DirectNutrient 1213 "Leucine" Milli

lysine :: MeasuredNutrient
lysine = Direct $ DirectNutrient 1214 "Lysine" Milli

methionine :: MeasuredNutrient
methionine = Direct $ DirectNutrient 1215 "Methionine" Milli

cystine :: MeasuredNutrient
cystine = Direct $ DirectNutrient 1216 "Cystine" Milli

phenylalanine :: MeasuredNutrient
phenylalanine = Direct $ DirectNutrient 1217 "Phenylalanine" Milli

tyrosine :: MeasuredNutrient
tyrosine = Direct $ DirectNutrient 1218 "Tyrosine" Milli

valine :: MeasuredNutrient
valine = Direct $ DirectNutrient 1219 "Valine" Milli

arginine :: MeasuredNutrient
arginine = Direct $ DirectNutrient 1220 "Arginine" Milli

histidine :: MeasuredNutrient
histidine = Direct $ DirectNutrient 1221 "Histidine" Milli

alanine :: MeasuredNutrient
alanine = Direct $ DirectNutrient 1222 "Alanine" Milli

asparticAcid :: MeasuredNutrient
asparticAcid = Direct $ DirectNutrient 1223 "Aspartic Acid" Milli

glutamicAcid :: MeasuredNutrient
glutamicAcid = Direct $ DirectNutrient 1224 "Glutamic Acid" Milli

glycine :: MeasuredNutrient
glycine = Direct $ DirectNutrient 1225 "Glycine" Milli

proline :: MeasuredNutrient
proline = Direct $ DirectNutrient 1226 "Proline" Milli

serine :: MeasuredNutrient
serine = Direct $ DirectNutrient 1227 "Serine" Milli

hydroxyproline :: MeasuredNutrient
hydroxyproline = Direct $ DirectNutrient 1228 "Hydroxyproline" Milli

asparagine :: MeasuredNutrient
asparagine = Direct $ DirectNutrient 1231 "Asparagine" Milli

cysteine :: MeasuredNutrient
cysteine = Direct $ DirectNutrient 1232 "Cysteine" Milli

glutamine :: MeasuredNutrient
glutamine = Direct $ DirectNutrient 1233 "Glutamine" Milli

taurine :: MeasuredNutrient
taurine = Direct $ DirectNutrient 1234 "Taurine" Milli

calcium :: MeasuredNutrient
calcium = Direct $ DirectNutrient 1087 "Calcium" Milli

iron :: MeasuredNutrient
iron = Direct $ DirectNutrient 1089 "Iron" Milli

magnesium :: MeasuredNutrient
magnesium = Direct $ DirectNutrient 1090 "Magnesium" Milli

phosphorus :: MeasuredNutrient
phosphorus = Direct $ DirectNutrient 1091 "Phosphorus" Milli

potassium :: MeasuredNutrient
potassium = Direct $ DirectNutrient 1092 "Potassium" Milli

sodium :: MeasuredNutrient
sodium = Direct $ DirectNutrient 1093 "Sodium" Milli

zinc :: MeasuredNutrient
zinc = Direct $ DirectNutrient 1095 "Zinc" Milli

copper :: MeasuredNutrient
copper = Direct $ DirectNutrient 1098 "Copper" Milli

iodine :: MeasuredNutrient
iodine = Direct $ DirectNutrient 1100 "Iodine" Milli

manganese :: MeasuredNutrient
manganese = Direct $ DirectNutrient 1101 "Manganese" Milli

molybdenum :: MeasuredNutrient
molybdenum = Direct $ DirectNutrient 1102 "Molybdenum" Milli

selenium :: MeasuredNutrient
selenium = Direct $ DirectNutrient 1103 "Selenium" Milli

vitaminB1 :: MeasuredNutrient
vitaminB1 = Direct $ DirectNutrient 1165 "Vitamin B1 (thiamine)" Milli

vitaminB2 :: MeasuredNutrient
vitaminB2 = Direct $ DirectNutrient 1166 "Vitamin B2 (riboflavin)" Milli

vitaminB3 :: MeasuredNutrient
vitaminB3 = Direct $ DirectNutrient 1167 "Vitamin B3 (niacin)" Milli

vitaminB5 :: MeasuredNutrient
vitaminB5 = Direct $ DirectNutrient 1170 "Vitamin B5 (Pantathenic acid)" Milli

vitaminB6 :: MeasuredNutrient
vitaminB6 = Direct $ DirectNutrient 1175 "Vitamin B6 (pyridoxine)" Milli

vitaminB7 :: MeasuredNutrient
vitaminB7 = Direct $ DirectNutrient 1176 "Vitamin B7 (biotin)" Milli

vitaminB9 :: MeasuredNutrient
vitaminB9 = Direct $ DirectNutrient 1177 "Vitamin B9 (folate)" Micro

vitaminB12 :: MeasuredNutrient
vitaminB12 = Direct $ DirectNutrient 1178 "Vitamin B12 (cobalamins)" Micro

data NutTree = NutTree
  { ntFractions :: Branches
  , ntUnmeasuredHeader :: SummedNutrient
  , ntUnmeasuredTree :: Maybe (Aggregation NutTree)
  }

type Branches = NonEmpty (Aggregation Node)

data Node
  = MeasuredHeader MeasuredNutrient NutTree
  | UnmeasuredHeader SummedNutrient Branches
  | Leaf MeasuredNutrient

otherSFAs :: SummedNutrient
otherSFAs = SummedNutrient "Other SFAs" Unity

otherTFAs :: SummedNutrient
otherTFAs = SummedNutrient "Other TFAs" Unity

otherMUFAs :: SummedNutrient
otherMUFAs = SummedNutrient "Other MUFAs" Unity

otherPUFAs :: SummedNutrient
otherPUFAs = SummedNutrient "Other PUFAs" Unity

otherLipids :: SummedNutrient
otherLipids = SummedNutrient "Other Lipids" Unity

otherProteinMass :: SummedNutrient
otherProteinMass = SummedNutrient "Other Protein Mass" Milli

otherCarbs :: SummedNutrient
otherCarbs = SummedNutrient "Other Carbs" Unity

otherFiberBySolubility :: SummedNutrient
otherFiberBySolubility = SummedNutrient "Other Fiber (unclassified solubility)" Unity

otherFiberByWeight :: SummedNutrient
otherFiberByWeight = SummedNutrient "Other Fiber (unclassified weight)" Unity

otherInorganics :: SummedNutrient
otherInorganics = SummedNutrient "Other Inorganics" Unity

totalSugars :: SummedNutrient
totalSugars = SummedNutrient "Total Sugars" Unity

pufa_18_2_other :: SummedNutrient
pufa_18_2_other = SummedNutrient "PUFA 18:2 (unclassified)" Unity

pufa_18_3_other :: SummedNutrient
pufa_18_3_other = SummedNutrient "PUFA 18:3 (unclassified)" Unity

pufa_20_2_other :: SummedNutrient
pufa_20_2_other = SummedNutrient "PUFA 20:2 (unclassified)" Unity

pufa_20_3_other :: SummedNutrient
pufa_20_3_other = SummedNutrient "PUFA 20:3 (unclassified)" Unity

pufa_20_5_other :: SummedNutrient
pufa_20_5_other = SummedNutrient "PUFA 20:5 (unclassified)" Unity

pufa_22_5_other :: SummedNutrient
pufa_22_5_other = SummedNutrient "PUFA 22:5 (unclassified)" Unity

pufa_22_6_other :: SummedNutrient
pufa_22_6_other = SummedNutrient "PUFA 22:6 (unclassified)" Unity

mufa_22_1_other :: SummedNutrient
mufa_22_1_other = SummedNutrient "MUFA 22:1 (unclassified)" Unity

-- TODO add Ergothioneine (which is technically an animo acid although likely not in proteins)

allPhytosterols :: NonEmpty MeasuredNutrient
allPhytosterols =
  stigmastadiene
    :| [ stigmastadiene
       , stigmasterol
       , campesterol
       , brassicasterol
       , betaSitosterol
       , campestanol
       , betaSitostanol
       , delta_5_avenasterol
       , delta_7_stigmastenol
       , otherPhytosterols
       , ergosterol
       , ergosta_7_enol
       , ergosta_7_22_dienol
       , ergosta_5_7_dienol
       ]

allAminoAcids :: NonEmpty MeasuredNutrient
allAminoAcids =
  tryptophan
    :| [ threonine
       , isoleucine
       , leucine
       , lysine
       , methionine
       , cystine
       , phenylalanine
       , tyrosine
       , valine
       , arginine
       , histidine
       , alanine
       , asparticAcid
       , glutamicAcid
       , glycine
       , proline
       , serine
       , hydroxyproline
       , asparagine
       , cysteine
       , glutamine
       ]

allMinerals :: NonEmpty MeasuredNutrient
allMinerals =
  calcium
    :| [ iron
       , magnesium
       , phosphorus
       , potassium
       , sodium
       , zinc
       , copper
       , iodine
       , manganese
       , molybdenum
       , selenium
       ]

allTFAs :: NonEmpty MeasuredNutrient
allTFAs =
  tfa_14_1
    :| [ tfa_16_1
       , tfa_17_1
       , tfa_18_1
       , tfa_18_2
       , tfa_18_3
       , tfa_20_1
       , tfa_22_1
       ]

allSugars :: NonEmpty MeasuredNutrient
allSugars =
  sucrose
    :| [ glucose
       , fructose
       , lactose
       , maltose
       , galactose
       , raffinose
       , stachyose
       , verbascose
       ]

allSFAs :: NonEmpty MeasuredNutrient
allSFAs =
  sfa_4_0
    :| [ sfa_5_0
       , sfa_6_0
       , sfa_7_0
       , sfa_8_0
       , sfa_9_0
       , sfa_10_0
       , sfa_11_0
       , sfa_12_0
       , sfa_14_0
       , sfa_15_0
       , sfa_16_0
       , sfa_17_0
       , sfa_18_0
       , sfa_20_0
       , sfa_21_0
       , sfa_22_0
       , sfa_23_0
       , sfa_24_0
       ]

nutHierarchy :: NutTree
nutHierarchy =
  NutTree
    { ntFractions =
        leaf water
          :| [ measuredLeaves protein otherProteinMass allAminoAcids
             , measuredLeaves ash otherInorganics allMinerals
             , unmeasuredLeaves phytosterols allPhytosterols
             , measured lipid $
                nutTree
                  otherLipids
                  ( leaf cholesterol
                      :| [ measuredLeaves tfas otherTFAs allTFAs
                         , measuredLeaves sfas otherSFAs allSFAs
                         , measured pufas $
                            nutTree otherPUFAs $
                              measuredLeaves
                                pufa_18_2
                                pufa_18_2_other
                                (pufa_18_2_CLA :| [pufa_18_2_n6_cc])
                                :| [ measuredLeaves
                                      pufa_18_3
                                      pufa_18_3_other
                                      (pufa_18_3_n3_ccc :| [pufa_18_3_n6_ccc, pufa_18_3i])
                                   , leaf pufa_18_4
                                   , unmeasuredLeaves
                                      pufa_20_2
                                      (pufa_20_2_n6_cc :| [])
                                   , measuredLeaves
                                      pufa_20_3
                                      pufa_20_3_other
                                      (pufa_20_3_n3 :| [pufa_20_3_n6, pufa_20_3_n9])
                                   , leaf pufa_20_4
                                   , unmeasuredLeaves
                                      pufa_20_5
                                      (pufa_20_5_n3 :| [])
                                   , leaf pufa_22_2
                                   , leaf pufa_22_3
                                   , leaf pufa_22_4
                                   , unmeasuredLeaves
                                      pufa_22_5
                                      (pufa_22_5_n3 :| [])
                                   , unmeasuredLeaves
                                      pufa_22_6
                                      (pufa_22_6_n3 :| [])
                                   ]
                         , measured mufas $
                            nutTree otherMUFAs $
                              leaf mufa_12_1
                                :| [ leaf mufa_14_1
                                   , leaf mufa_15_1
                                   , leaf mufa_16_1
                                   , leaf mufa_17_1
                                   , leaf mufa_18_1
                                   , leaf mufa_20_1
                                   , measuredLeaves mufa_22_1 mufa_22_1_other $
                                      mufa_22_1_n11 :| [mufa_22_1_n9]
                                   ]
                         ]
                  )
             ]
    , ntUnmeasuredHeader = carbDiff
    , ntUnmeasuredTree =
        Just $
          Single $
            nutTree otherCarbs $
              leaf starch
                :| [ leaf betaGlucan
                   , unmeasured totalSugars $ leaf <$> allSugars
                   , Priority
                      ( MeasuredHeader
                          fiberBySolubility
                          ( nutTree otherFiberBySolubility $
                              fmap leaf (solubleFiber :| [insolubleFiber])
                          )
                          :| [ MeasuredHeader
                                fiberByWeight
                                ( nutTree otherFiberByWeight $
                                    fmap leaf (highMWFiber :| [lowMWFiber])
                                )
                             ]
                      )
                   ]
    }
  where
    leaf = Single . Leaf
    measured h = Single . MeasuredHeader h
    unmeasured h = Single . UnmeasuredHeader h
    nutTree u xs =
      NutTree
        { ntFractions = xs
        , ntUnmeasuredHeader = u
        , ntUnmeasuredTree = Nothing
        }
    measuredLeaves h u xs = measured h $ nutTree u (leaf <$> xs)
    unmeasuredLeaves h xs = unmeasured h (leaf <$> xs)

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
  deriving (Show, Eq, Ord)

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
    NutrientError
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)
