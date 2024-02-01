{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Csv as C
import qualified Data.Map.Merge.Strict as MMS
import Data.Monoid
import Data.Scientific
import GHC.Generics
import Internal.Types.Dhall
import Internal.Types.FoodItem
import RIO
import qualified RIO.Map as M
import RIO.State
import qualified RIO.Text as T
import RIO.Time

type NutrientMap = M.Map NID ValidNutrient

type MappedFoodItem = FoodItem NutrientMap

type ValidCustomMap = Map Text MappedFoodItem

data ValidNutrient = ValidNutrient
  { vnAmount :: Mass
  , vnPrefix :: Prefix
  , vnName :: Maybe Text
  }
  deriving (Show)

data CLIOptions = CLIOptions CommonOptions SubCommand

newtype CommonOptions = CommonOptions
  { coVerbosity :: Bool
  }

newtype APIKey = APIKey {unAPIKey :: Text} deriving (IsString) via Text

data SubCommand
  = Fetch !FetchDumpOptions
  | Dump !FetchDumpOptions
  | ExportTabular !TabularExportOptions
  | ExportTree !TreeExportOptions
  | ListNutrients
  | Summarize !ExportOptions

data FetchDumpOptions = FetchDumpOptions
  { foID :: !FID
  , foForce :: !Bool
  , foKey :: !(Maybe APIKey)
  }

-- TODO what does this name really mean?
data CommonExportOptions = CommonExportOptions
  { ceoExport :: !ExportOptions
  , ceoGroup :: !GroupOptions
  , ceoShowUnknowns :: !Bool
  , ceoUnityUnits :: !Bool
  }

data TabularExportOptions = TabularExportOptions
  { tabCommonExport :: !CommonExportOptions
  , tabSort :: !Text
  }

data TreeExportOptions = TreeExportOptions
  { treeDisplay :: !TreeDisplayOptions
  , treeJSON :: !Bool
  , treeCommonExport :: !CommonExportOptions
  }

data ExportOptions = ExportOptions
  { eoMealPath :: !FilePath
  , eoDateInterval :: !DateIntervalOptions
  , eoForce :: !Bool
  , eoThreads :: !Int
  , eoKey :: !(Maybe APIKey)
  , eoRoundDigits :: !Int
  }

data GroupOptions = GroupOptions
  { goDate :: !Bool
  , goMeal :: !Bool
  , goIngredient :: !Bool
  }

data DateIntervalOptions = DateIntervalOptions
  { dioStart :: !(Maybe Day)
  , dioEnd :: !(Maybe Day)
  , dioDays :: !Int
  , dioInterval :: !(Maybe Int)
  , dioNormalize :: !Int
  }

newtype TreeDisplayOptions = TreeDisplayOptions
  { doExpandedUnits :: Bool
  }

data AllTreeDisplayOptions = AllTreeDisplayOptions
  { atdoOpts :: !TreeDisplayOptions
  , atdoShowUnknowns :: !Bool
  , atdoUnityUnits :: !Bool
  , atdoRoundDigits :: !Int
  }

data AllTabularDisplayOptions = AllTabularDisplayOptions
  { atabShowUnknowns :: !Bool
  , atabUnityUnits :: !Bool
  , atabRoundDigits :: !Int
  , atabSort :: ![SortKey]
  }

type TableSort = forall r. DisplayRow r -> DisplayRow r -> Bool

data SortKey = SortKey
  { skField :: SortField
  , skAsc :: Bool
  }
  deriving (Eq)

data SortField
  = SortDate
  | SortMeal
  | SortIngredient
  | SortNutrient
  | SortParent
  | SortValue
  deriving (Eq)

type DaySpan = (Day, Int)

data ValidSchedule = ValidSchedule
  { vsIngs :: NonEmpty Ingredient
  , vsMeal :: MealGroup
  , vsCron :: Cron
  , vsScale :: Scientific
  }

type IngredientMetadata = IngredientMetadata_ [Modification] DaySpan

-- TODO bad name
type IngredientMealMeta = IngredientMetadata_ () Day

data IngredientMetadata_ ms d = IngredientMetadata_
  { imMeal :: MealGroup
  , imMass :: Mass
  , imMods :: ms
  , imDaySpan :: d
  }

data SummaryRow = SummaryRow
  { srDay :: Day
  , srMeal :: MealGroup
  , srIngredient :: IngredientGroup
  , srMass :: Mass
  }

summaryRowHeader :: [ByteString]
summaryRowHeader = ["day", "meal", "ingredient", "mass"]

instance C.DefaultOrdered SummaryRow where
  headerOrder _ = C.header summaryRowHeader

instance C.ToNamedRecord SummaryRow where
  toNamedRecord (SummaryRow d m i v) =
    zipApply summaryRowHeader [(C..= formatDay d), (C..= m), (C..= i), (C..= v)]

data ValidFDCIngredient = ValidFDCIngredient
  { viID :: FID
  , viMass :: Mass
  , viModifications :: [Modification]
  }

data UnitName
  = Gram
  | Calorie
  deriving (Show, Eq, Ord, Generic, ToJSON)

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
unitSymbol Gram = "g"

tunit :: Unit -> Text
tunit (Unit p n) = T.append (prefixSymbol p) (unitSymbol n)

instance C.ToField Unit where
  toField = encodeUtf8 . tunit

type NutrientReader = MonadReader FoodMeta

data FoodState = FoodState
  { fsNutrients :: [FoodNutrient]
  , fsWarnings :: [NutrientWarning]
  }

-- TODO add name to this so that the user is less confused
data NutrientWarning
  = NotGram !NID !Text
  | UnknownUnit !NID !Text
  | InvalidNutrient !FoodNutrient

type NutrientState = MonadState NutrientMap

type MealState = MonadState [NutrientWarning]

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

-- TODO we don't have id for custom ingredients
data FoodMeta = FoodMeta
  { fmDesc :: Text
  , fmId :: Maybe FID
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

-- data SpanFood a b = SpanFood
--   { sfFinal :: DisplayTree a b
--   , sfDaySpan :: DaySpan
--   }
--   deriving (Generic, Show)

-- type FinalScalerFood = DisplayTree Mass Energy

-- type FinalGroupedFood = DisplayTree NutrientMass NutrientEnergy

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

--------------------------------------------------------------------------------
-- display tree

-- Types for exporting nutrient date in a tree-like format. This is polymorphic
-- to encode the possible groupings for the output table. The grouping variables
-- include date, meal, and ingredient. In the case of date, "grouping" means
-- collecting dates within a given range.

newtype MealGroup = MealGroup {unMealGroup :: Text}
  deriving (Show, Eq, Ord, ToJSON, C.ToField) via Text

newtype IngredientGroup = IngredientGroup {unIngredientGroup :: Text}
  deriving (Show, Eq, Ord, ToJSON, C.ToField) via Text

data GroupVars d m i = GroupVars
  { gvDaySpan :: d
  , gvMeal :: m
  , gvIngredient :: i
  }
  deriving (Eq, Generic)

type GroupByNone = GroupVars () () ()

type GroupByDate = GroupVars DaySpan () ()

type GroupByMeal = GroupVars () MealGroup ()

type GroupByIngredient = GroupVars () () IngredientGroup

type GroupByDateMeal = GroupVars DaySpan MealGroup ()

type GroupByDateIngredient = GroupVars DaySpan () IngredientGroup

type GroupByMealIngredient = GroupVars () MealGroup IngredientGroup

type GroupByAll = GroupVars DaySpan MealGroup IngredientGroup

instance ToJSON GroupByNone where
  toJSON _ = object []

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByDate where
  toJSON (GroupVars d _ _) = object [daySpanJSON d]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByMeal where
  toJSON (GroupVars _ m _) = object [mealJSON m]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByIngredient where
  toJSON (GroupVars _ _ i) = object [ingredientJSON i]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByDateMeal where
  toJSON (GroupVars d m _) = object [daySpanJSON d, mealJSON m]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByDateIngredient where
  toJSON (GroupVars d _ i) = object [daySpanJSON d, ingredientJSON i]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByMealIngredient where
  toJSON (GroupVars _ m i) = object [mealJSON m, ingredientJSON i]

  toEncoding = genericToEncoding defaultOptions

instance ToJSON GroupByAll where
  toJSON (GroupVars d m i) = object [daySpanJSON d, mealJSON m, ingredientJSON i]

  toEncoding = genericToEncoding defaultOptions

daySpanJSON :: DaySpan -> Pair
daySpanJSON ds =
  let (s, e) = fromDaySpan ds
   in "span" .= object ["start" .= s, "end" .= e]

mealJSON :: MealGroup -> Pair
mealJSON = (.=) "meal"

ingredientJSON :: IngredientGroup -> Pair
ingredientJSON = (.=) "ingredient"

data DisplayTree_ g a b = DisplayTree_
  { ffMap :: DisplayNode a
  , ffEnergy :: b
  , ffGroup :: g
  }
  deriving (Generic, Show)

-- fmap cheat code: make mass and energy polymorphic so I don't need to use
-- a lens to "map" over this structure
instance Bifunctor (DisplayTree_ g) where
  bimap f g r@(DisplayTree_ as b _) = r {ffMap = f <$> as, ffEnergy = g b}

type DisplayTreeSum = DisplayTree_ () (Sum Mass) (Sum Energy)

-- only allow "adding" together if there is no grouping data to clobber
instance Semigroup (DisplayTree_ () (Sum Mass) (Sum Energy)) where
  (<>) a b = DisplayTree_ (ffMap a <> ffMap b) (ffEnergy a + ffEnergy b) ()

type DisplayTree g = DisplayTree_ g Mass Energy

--------------------------------------------------------------------------------
-- display rows

-- Types for exporting nutrient date in a csv-like format. This is analogous
-- to DisplayTree above

data DisplayRow g = DisplayRow
  { drGroup :: g
  , drNutrient :: Text
  , drParentNutrient :: Maybe Text
  , drValue :: Scientific
  , drUnit :: Unit
  }
  deriving (Show, Generic)

nutrientHeaders :: [ByteString]
nutrientHeaders = ["nutrient", "parent", "value", "unit"]

dateHeaders :: [ByteString]
dateHeaders = ["start", "end"]

mealHeader :: ByteString
mealHeader = "meal"

ingredientHeader :: ByteString
ingredientHeader = "ingredient"

byNoneHeader :: [ByteString]
byNoneHeader = mempty -- TODO is this legal?

byDateHeader :: [ByteString]
byDateHeader = dateHeaders

byMealHeader :: [ByteString]
byMealHeader = [mealHeader]

byIngredientHeader :: [ByteString]
byIngredientHeader = [ingredientHeader]

byDateMealHeader :: [ByteString]
byDateMealHeader = dateHeaders ++ [mealHeader]

byDateIngredientHeader :: [ByteString]
byDateIngredientHeader = dateHeaders ++ [ingredientHeader]

byMealIngredientHeader :: [ByteString]
byMealIngredientHeader = [mealHeader, ingredientHeader]

byAllHeader :: [ByteString]
byAllHeader = dateHeaders ++ [mealHeader, ingredientHeader]

instance C.DefaultOrdered (DisplayRow GroupByNone) where
  headerOrder _ = displayRowHeader byNoneHeader

instance C.DefaultOrdered (DisplayRow GroupByDate) where
  headerOrder _ = displayRowHeader byDateHeader

instance C.DefaultOrdered (DisplayRow GroupByMeal) where
  headerOrder _ = displayRowHeader byMealHeader

instance C.DefaultOrdered (DisplayRow GroupByIngredient) where
  headerOrder _ = displayRowHeader byIngredientHeader

instance C.DefaultOrdered (DisplayRow GroupByDateMeal) where
  headerOrder _ = displayRowHeader byDateMealHeader

instance C.DefaultOrdered (DisplayRow GroupByDateIngredient) where
  headerOrder _ = displayRowHeader byDateIngredientHeader

instance C.DefaultOrdered (DisplayRow GroupByMealIngredient) where
  headerOrder _ = displayRowHeader byMealIngredientHeader

instance C.DefaultOrdered (DisplayRow GroupByAll) where
  headerOrder _ = displayRowHeader byAllHeader

displayRowHeader :: [ByteString] -> C.Header
displayRowHeader hs = C.header $ hs ++ nutrientHeaders

instance C.ToNamedRecord (DisplayRow GroupByNone) where
  toNamedRecord r = nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByDate) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars d _ _)} =
    zipApply byDateHeader (daySpanCsv d) <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByMeal) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars _ m _)} =
    zipApply byMealHeader [(C..= m)] <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByIngredient) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars _ _ i)} =
    zipApply byIngredientHeader [(C..= i)] <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByDateMeal) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars d m _)} =
    zipApply byDateMealHeader (daySpanCsv d ++ [(C..= m)]) <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByDateIngredient) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars d _ i)} =
    zipApply byDateIngredientHeader (daySpanCsv d ++ [(C..= i)])
      <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByMealIngredient) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars _ m i)} =
    zipApply byMealIngredientHeader [(C..= m), (C..= i)] <> nutrientRecord r

instance C.ToNamedRecord (DisplayRow GroupByAll) where
  toNamedRecord r@DisplayRow {drGroup = (GroupVars d m i)} =
    zipApply byAllHeader (daySpanCsv d ++ [(C..= m), (C..= i)])
      <> nutrientRecord r

nutrientRecord :: DisplayRow g -> C.NamedRecord
nutrientRecord (DisplayRow _ n p v u) =
  zipApply nutrientHeaders [(C..= n), (C..= p), (C..= v), (C..= u)]

type PartialField = ByteString -> (ByteString, ByteString)

zipApply :: [ByteString] -> [PartialField] -> C.NamedRecord
zipApply hs = C.namedRecord . zipWith (\h f -> f h) hs

formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

fromDaySpan :: DaySpan -> (Day, Day)
fromDaySpan (d, n) = (d, addDays (fromIntegral n + 1) d)

daySpanCsv :: DaySpan -> [PartialField]
daySpanCsv ds =
  let (s, e) = fromDaySpan ds
   in [(C..= formatDay s), (C..= formatDay e)]

data PrefixValue = PrefixValue {pvPrefix :: Prefix, pvX :: Scientific}

newtype Energy = Energy {unEnergy :: Scientific}
  deriving (Show, Eq, Ord, Num, ToJSON, Fractional, Real, RealFrac) via Scientific

data NutTreeRow = NutTreeRow
  { ntrNutrient :: Text
  , ntrParent :: Maybe Text
  , ntrId :: Maybe NID
  }

nutTreeRowHeader :: [ByteString]
nutTreeRowHeader = ["nutrient", "parent", "id"]

instance C.DefaultOrdered NutTreeRow where
  headerOrder _ = C.header nutTreeRowHeader

instance C.ToNamedRecord NutTreeRow where
  toNamedRecord (NutTreeRow n p i) =
    zipApply nutTreeRowHeader [(C..= n), (C..= p), (C..= i)]

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
  | CustomIngError !CustomIngError
  | MissingCustom !Text
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)

data CustomIngError
  = CustomDups Text (NonEmpty NID)
  | TooMuchMass Text
  deriving (Show)

data UnusedNutrient = UnusedNutrient
  { uMeal :: MealGroup
  , uId :: NID
  , uNut :: ValidNutrient
  }
