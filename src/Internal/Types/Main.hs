{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types as AT
import qualified Data.Csv as C
import qualified Data.Map.Merge.Strict as MMS
import Data.Monoid
import Data.Monoid.Generic
import Data.Scientific
import GHC.Generics
import Internal.Types.Dhall
import RIO
import qualified RIO.Char as C
import qualified RIO.List as L
import qualified RIO.Map as M
import RIO.State
import qualified RIO.Text as T
import RIO.Time

data Options = Options CommonOptions SubCommand

data CommonOptions = CommonOptions
  { coKey :: !(Maybe APIKey)
  , coVerbosity :: !Bool
  }

newtype FID = FID {unFID :: Int}
  deriving (Eq, Read, Show, FromJSON, ToJSON) via Int

newtype NID = NID {unNID :: Int}
  deriving (Read, Show, FromJSON, ToJSON, Eq, Ord, Num) via Int

newtype APIKey = APIKey {unAPIKey :: Text} deriving (IsString) via Text

data SubCommand
  = Fetch !FetchDumpOptions
  | Dump !FetchDumpOptions
  | Export !ExportOptions
  | Summarize !SummarizeOptions

data FetchDumpOptions = FetchDumpOptions {foID :: !FID, foForce :: !Bool}

data ExportOptions = ExportOptions
  { eoMealPath :: !FilePath
  , eoDateInterval :: !DateIntervalOptions
  , eoForce :: !Bool
  , eoThreads :: !Int
  }

data SummarizeOptions = SummarizeOptions
  { soDisplay :: !DisplayOptions
  , soJSON :: !Bool
  , soExport :: !ExportOptions
  }

data DateIntervalOptions = DateIntervalOptions
  { dioStart :: Maybe Day
  , dioEnd :: Maybe Day
  , dioDays :: Int
  , dioInterval :: Maybe Int
  , dioNormalize :: Int
  }

data DisplayOptions = DisplayOptions
  { doUnknowns :: !Bool
  , doMembers :: !Bool
  , doExpandedUnits :: !Bool
  , doUnityUnits :: !Bool
  }

type DaySpan = (Day, Int)

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

data ProteinConversion = ProteinConversion
  { pcFactor :: Scientific
  , pcAssumed :: Bool -- TODO why did I want this?
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
    defProtein = ProteinConversion 6.25 True

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
      return $ Just $ ProteinConversion p False
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

data ValidSchedule a = ValidSchedule
  { vsIngs :: NonEmpty Ingredient
  , vsMeta :: a
  }

stripRecordPrefix :: String -> String -> String
stripRecordPrefix prefix = maybe [] go . L.stripPrefix prefix
  where
    go [] = []
    go (x : xs) = C.toLower x : xs

recordOptions :: String -> AT.Options
recordOptions x =
  defaultOptions
    { fieldLabelModifier = stripRecordPrefix x
    , rejectUnknownFields = False
    }

recordParseJSON :: (Generic a, GFromJSON Zero (Rep a)) => String -> Value -> Parser a
recordParseJSON s = genericParseJSON (recordOptions s)

data UnitName
  = Gram
  | Calorie
  | Joule
  | IU
  deriving (Show, Eq, Generic, ToJSON)

data Unit = Unit
  { unitBase :: Prefix
  , unitName :: UnitName
  }
  deriving (Show, Eq, Generic, ToJSON)

prefixValue :: Prefix -> Int
prefixValue Nano = -9
prefixValue Micro = -6
prefixValue Milli = -3
prefixValue Centi = -2
prefixValue Deci = -1
prefixValue Unity = 0
prefixValue Deca = 1
prefixValue Hecto = 2
prefixValue Kilo = 3
prefixValue Mega = 6
prefixValue Giga = 9

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

unitSymbol :: UnitName -> Text
unitSymbol Calorie = "cal"
unitSymbol Joule = "J"
unitSymbol Gram = "g"
unitSymbol IU = "IU"

tunit :: Unit -> Text
tunit (Unit p n) = T.append (prefixSymbol p) (unitSymbol n)

instance C.ToField Unit where
  toField = encodeUtf8 . tunit

type NutrientReader = MonadReader FoodMeta

data FoodState = FoodState
  { fsNutrients :: [FoodNutrient]
  , fsWarnings :: [AppWarning]
  }

data AppWarning = AppWarning
  { awType :: !AppWarningType
  , awId :: !NID
  }

data AppWarningType
  = NotGram
  | NoUnit
  | NoAmount

type NutrientState = MonadState FoodState

type MealState = MonadState [AppWarning]

data MeasuredNutrient
  = Direct DirectNutrient
  | -- | Computed ComputedNutrient
    Alternate AltNutrient
  deriving (Show, Eq, Ord)

data DirectNutrient = DirectNutrient
  { mnId :: NID
  , mnName :: Text
  , mnDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

data AltNutrient = AltNutrient
  { anName :: Text
  , anDisplayPrefix :: Prefix
  , anChoices :: NonEmpty (NID, Maybe Scientific)
  }
  deriving (Show, Eq, Ord)

data SummedNutrient = SummedNutrient
  { snName :: Text
  , snDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

data DisplayNutrient = DisplayNutrient {dnName :: Text, dnPrefix :: Prefix}
  deriving (Show, Eq, Ord)

data FoodMeta = FoodMeta
  { fmDesc :: Text
  , fmId :: FID
  }
  deriving (Show, Generic, ToJSON)

data FoodTreeNode a = FullNode (FullNode_ a) | PartialNode (PartialNode_ a)
  deriving (Functor)

data FullNode_ a = FullNode_
  { fnValue :: a
  -- ^ Mass of this node
  , fnNut :: DisplayNutrient
  -- ^ Nutrient associated with this node
  , fnKnown :: [FoodTreeNode a]
  -- ^ Subnutrients underneath this node with known mass
  , fnUnknown :: Either [UnknownTree] (FullNode_ a)
  -- ^ Subnutrients underneath this node with no known individual masses but
  -- known collective masses. This mass and all those under the "known" field
  -- must sum to that of the "value" field
  }
  deriving (Functor)

data PartialNode_ a = PartialNode_
  { pnNut :: DisplayNutrient
  -- ^ Nutrient associated with this node
  , pnKnown :: NonEmpty (FoodTreeNode a)
  -- ^ Subnutrients underneath this node with known mass
  }
  deriving (Functor)

data UnknownTree = UnknownTree Text [UnknownTree]
  deriving (Eq, Ord, Show)

instance ToJSON UnknownTree where
  toJSON (UnknownTree n ts) = object ["name" .= n, "children" .= ts]

data SpanFood = SpanFood
  { sfFinal :: FinalFood_ NutrientValue
  , sfDaySpan :: DaySpan
  }
  deriving (Generic, Show)

data FinalFood_ a = FinalFood_
  { ffMap :: DisplayNode a
  , ffEnergy :: a
  }
  deriving (Generic, Functor, Show)
  deriving (Semigroup) via GenericSemigroup (FinalFood_ a)

type FinalFood = FinalFood_ NutrientValue

data DisplayNode a = DisplayNode
  { dnValue :: a
  , dnKnown :: M.Map DisplayNutrient (DisplayNode a)
  , dnUnknown :: M.Map [UnknownTree] a
  }
  deriving (Functor, Show)

instance Semigroup a => Semigroup (DisplayNode a) where
  (<>) a b =
    DisplayNode
      { dnValue = dnValue a <> dnValue b
      , dnKnown = merge_ (dnKnown a) (dnKnown b)
      , dnUnknown = merge_ (dnUnknown a) (dnUnknown b)
      }
    where
      merge_ :: (Ord k, Semigroup v) => M.Map k v -> M.Map k v -> M.Map k v
      merge_ =
        MMS.merge
          MMS.preserveMissing
          MMS.preserveMissing
          (MMS.zipWithMatched (\_ x y -> x <> y))

data DisplayRow = DisplayRow
  { drStart :: Day
  , drEnd :: Day
  , drNutrient :: Text
  , drParentNutrient :: Maybe Text
  , drValue :: Scientific
  , drUnit :: Unit
  }
  deriving (Show, Generic)

drHeader :: [ByteString]
drHeader =
  [ "start"
  , "end"
  , "nutrient"
  , "parent"
  , "value"
  , "unit"
  ]

instance C.DefaultOrdered DisplayRow where
  headerOrder _ = C.header drHeader

instance C.ToNamedRecord DisplayRow where
  toNamedRecord (DisplayRow s e n p v u) =
    C.namedRecord $
      zipWith
        (\h f -> f h)
        drHeader
        [(C..= fmt s), (C..= fmt e), (C..= n), (C..= p), (C..= v), (C..= u)]
    where
      fmt = formatTime defaultTimeLocale "%Y-%m-%d"

data PrefixValue = PrefixValue {pvPrefix :: Prefix, pvX :: Scientific}

type NutrientValue = NutrientValue_ (Sum Scientific)

data NutrientValue_ a = NutrientValue
  { nvValue :: a
  , -- TODO non empty set here
    nvMembers :: NonEmpty FoodMeta
  }
  deriving (Show, Generic, Functor)
  deriving (Semigroup) via GenericSemigroup (NutrientValue_ a)

instance ToJSON NutrientValue where
  toJSON (NutrientValue (Sum v) ms) =
    object ["value" .= v, "members" .= ms]

  toEncoding (NutrientValue (Sum v) ms) =
    pairs ("value" .= v <> "members" .= ms)

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

data Aggregation a = AggIdentity a | Priority (NonEmpty a)

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
  deriving (Show, Eq, Ord, Enum, Bounded, Generic, ToJSON)

instance Exception AppException

newtype AppException = AppException [AppError]
  deriving (Show, Semigroup) via [AppError]

type MonadAppError = MonadError AppException

type AppExcept = AppExceptT Identity

type AppExceptT = ExceptT AppException

data AppError
  = DatePatternError !Natural !Natural !(Maybe Natural) !PatternSuberr
  | DaySpanError !Int
  | IntervalError !Int
  | JSONError !ByteString
  | EmptyMeal !T.Text
  | MissingAPIKey !FilePath
  | FileTypeError !FilePath
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)
