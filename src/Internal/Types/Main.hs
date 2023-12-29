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

data FoodState = FoodState
  { fsNutrients :: [FoodNutrient]
  , fsWarnings :: [AppWarning]
  }

data AppWarning
  = NotGram
  | NoUnit
  | NoAmount

type NutrientState = MonadState FoodState

data MeasuredNutrient
  = Direct DirectNutrient
  | -- | Computed ComputedNutrient
    Alternate AltNutrient
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
  , anChoices :: NonEmpty (Int, Maybe Scientific)
  }
  deriving (Show, Eq, Ord)

data SummedNutrient = SummedNutrient
  { snName :: T.Text
  , snDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

data DisplayNutrient = DisplayNutrient {dnName :: T.Text, dnPrefix :: Prefix}
  deriving (Show, Eq, Ord)

data FoodMeta = FoodMeta
  { fmDesc :: T.Text
  , fmId :: Int
  , fmNitrogenFactor :: Scientific
  }
  deriving (Show)

data FoodTreeNode a = FoodTreeNode
  { ftValue :: a
  -- ^ Mass of this node
  , ftNut :: DisplayNutrient
  -- ^ Nutrient associated with this node
  , ftKnown :: [FoodTreeNode a]
  -- ^ Subnutrients underneath this node with known mass
  , ftUnknown :: [UnknownTree]
  -- ^ Subnutrients underneath this node with no known individual masses but
  -- known collective masses. This mass and all those under the "known" field
  -- must sum to that of the "value" field
  }
  deriving (Functor)

data UnknownTree = UnknownTree DisplayNutrient [UnknownTree]

-- data FoodTree a = NutrientNode (FoodTreeNode a) | GroupNode T.Text [FoodTree a]

data PrefixValue = PrefixValue {pvPrefix :: Prefix, pvX :: Scientific}

type NutrientValue = NutrientValue_ (Sum Scientific)

data NutrientValue_ a = NutrientValue
  { nvValue :: a
  , -- TODO non empty set here
    nvMembers :: NonEmpty FoodMeta
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (NutrientValue_ a)

-- | A group of nutrient categories that represent an aggregate mass
data NutTree = NutTree
  { ntFractions :: Branches
  -- ^ Categories (at least one) that sum to a known mass. The mass of these
  -- categories may be 1) a known mass 2) a known mass with additional
  -- subcategories underneath 3) an unknown mass determined by subcategories
  -- beneath it or 4) a placeholder with at least one of 1-3 underneath it.
  , ntUnmeasuredHeader :: SummedNutrient
  -- ^ The header for the one unmeasured category (ie the total mass represented
  -- by this entire type minus the sum of all fractions)
  , ntUnmeasuredTree :: Maybe NutTree
  -- ^ An optional tree by which the unmeasured category may be subdivided. If
  -- there is no tree, then the unmeasured category is simply a "leaf" (ie
  -- nothing under it)
  }

type Branches = NonEmpty (Aggregation Node)

data Node
  = MeasuredHeader MeasuredNutrient NutTree
  | UnmeasuredHeader SummedNutrient Branches
  | -- | GroupHeader SummedNutrient Branches
    Leaf MeasuredNutrient

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
