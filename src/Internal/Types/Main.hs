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
import qualified RIO.Vector as V

type NutrientMap = M.Map NID ValidNutrient

type MappedFoodItem = FoodItem NutrientMap

type ValidCustomMap = Map Text MappedFoodItem

data ValidNutrient = ValidNutrient
  { vnAmount :: Mass
  , vnPrefix :: Prefix
  }
  deriving (Show)

data CLIOptions = CLIOptions CommonOptions SubCommand

data CommonOptions = CommonOptions
  { coKey :: !(Maybe APIKey)
  , coVerbosity :: !Bool
  }

newtype APIKey = APIKey {unAPIKey :: Text} deriving (IsString) via Text

data SubCommand
  = Fetch !FetchDumpOptions
  | Dump !FetchDumpOptions
  | ExportTabular !TabularOptions
  | ExportTree !TreeOptions

data FetchDumpOptions = FetchDumpOptions {foID :: !FID, foForce :: !Bool}

data TabularOptions = TabularOptions
  { eoMealPath :: !FilePath
  , eoDateInterval :: !DateIntervalOptions
  , eoForce :: !Bool
  , eoThreads :: !Int
  , eoGroup :: !GroupOptions
  }

data TreeOptions = TreeOptions
  { soDisplay :: !DisplayOptions
  , soJSON :: !Bool
  , soExport :: !TabularOptions
  }

data GroupOptions = GroupOptions
  { goDate :: Bool
  , goMeal :: Bool
  , goIngredient :: Bool
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
  , doExpandedUnits :: !Bool
  , doUnityUnits :: !Bool
  }

type DaySpan = (Day, Int)

data ValidSchedule = ValidSchedule
  { vsIngs :: NonEmpty Ingredient
  , vsMeal :: MealGroup
  , vsCron :: Cron
  , vsScale :: Scientific
  }

data IngredientMetadata = IngredientMetadata
  { imMeal :: MealGroup
  , imMass :: Mass
  , imMods :: [Modification]
  , imDaySpan :: DaySpan
  }

-- data CustomIngredient = CustomIngredient CustomSource Mass [Modification]

data ValidFDCIngredient = ValidFDCIngredient
  { viID :: FID
  , viMass :: Mass
  , viModifications :: [Modification]
  }

data UnitName
  = Gram
  | Calorie
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
  deriving (Show, Eq, ToJSON, C.ToField) via Text

newtype IngredientGroup = IngredientGroup {unIngredientGroup :: Text}
  deriving (Show, Eq, ToJSON, C.ToField) via Text

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

dateHeaders :: [ByteString]
dateHeaders = ["start", "end"]

mealHeader :: ByteString
mealHeader = "meal"

ingredientHeader :: ByteString
ingredientHeader = "ingredient"

instance C.DefaultOrdered GroupByNone where
  headerOrder _ = C.header mempty -- TODO is this legal?

instance C.DefaultOrdered GroupByDate where
  headerOrder _ = C.header dateHeaders

instance C.DefaultOrdered GroupByMeal where
  headerOrder _ = C.header [mealHeader]

instance C.DefaultOrdered GroupByIngredient where
  headerOrder _ = C.header [ingredientHeader]

instance C.DefaultOrdered GroupByDateMeal where
  headerOrder _ = C.header $ dateHeaders ++ [mealHeader]

instance C.DefaultOrdered GroupByDateIngredient where
  headerOrder _ = C.header [mealHeader, ingredientHeader]

instance C.DefaultOrdered GroupByMealIngredient where
  headerOrder _ = C.header $ dateHeaders ++ [ingredientHeader]

instance C.DefaultOrdered GroupByAll where
  headerOrder _ = C.header $ dateHeaders ++ [mealHeader, ingredientHeader]

instance C.ToNamedRecord GroupByNone where
  toNamedRecord _ = C.namedRecord [] -- TODO this seems odd

instance C.ToNamedRecord GroupByDate where
  toNamedRecord r@(GroupVars d _ _) = zipApplyV r $ daySpanCsv d

instance C.ToNamedRecord GroupByMeal where
  toNamedRecord r@(GroupVars _ m _) = zipApplyV r [(C..= m)]

instance C.ToNamedRecord GroupByIngredient where
  toNamedRecord r@(GroupVars _ _ i) = zipApplyV r [(C..= i)]

instance C.ToNamedRecord GroupByDateMeal where
  toNamedRecord r@(GroupVars d m _) = zipApplyV r $ daySpanCsv d ++ [(C..= m)]

instance C.ToNamedRecord GroupByDateIngredient where
  toNamedRecord r@(GroupVars d _ i) = zipApplyV r $ daySpanCsv d ++ [(C..= i)]

instance C.ToNamedRecord GroupByMealIngredient where
  toNamedRecord r@(GroupVars _ m i) = zipApplyV r [(C..= m), (C..= i)]

instance C.ToNamedRecord GroupByAll where
  toNamedRecord r@(GroupVars d m i) =
    zipApplyV r $ daySpanCsv d ++ [(C..= m), (C..= i)]

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

-- type DisplayTreeNoGroup = DisplayTree () () ()

-- type DisplayTreeByDate = DisplayTree DaySpan () ()

-- type DisplayTreeByMeal = DisplayTree () MealGroup ()

-- type DisplayTreeByIngredient = DisplayTree () () IngredientGroup

-- type DisplayTreeByDateMeal = DisplayTree DaySpan MealGroup ()

-- type DisplayTreeByDateIngredient = DisplayTree DaySpan () IngredientGroup

-- type DisplayTreeByMealIngredient = DisplayTree () MealGroup IngredientGroup

-- type DisplayTreeByAll = DisplayTree DaySpan MealGroup IngredientGroup

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

instance C.DefaultOrdered g => C.DefaultOrdered (DisplayRow g) where
  headerOrder DisplayRow {drGroup} =
    C.headerOrder drGroup <> C.header nutrientHeaders

instance C.ToNamedRecord g => C.ToNamedRecord (DisplayRow g) where
  toNamedRecord (DisplayRow g n p v u) = gr <> nr
    where
      gr = C.toNamedRecord g
      nr = zipApply nutrientHeaders [(C..= n), (C..= p), (C..= v), (C..= u)]

type PartialField = ByteString -> (ByteString, ByteString)

zipApply :: [ByteString] -> [PartialField] -> C.NamedRecord
zipApply hs = C.namedRecord . zipWith (\h f -> f h) hs

zipApplyV :: C.DefaultOrdered a => a -> [PartialField] -> C.NamedRecord
zipApplyV r = zipApply (V.toList (C.headerOrder r))

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
  deriving (Show, Eq, Ord, Num, ToJSON, Fractional) via Scientific

-- type NutrientMass = NutrientValue_ (Sum Mass)

-- type NutrientEnergy = NutrientValue_ (Sum Energy)

-- data NutrientValue_ a = NutrientValue
--   { nvValue :: a
--   , -- TODO non empty set here
--     nvMembers :: NonEmpty FoodMeta
--   }
--   deriving (Show, Generic, Functor)
--   deriving (Semigroup) via GenericSemigroup (NutrientValue_ a)

-- instance ToJSON v => ToJSON (NutrientValue_ (Sum v)) where
--   toJSON (NutrientValue (Sum v) ms) =
--     object ["value" .= v, "members" .= ms]

--   toEncoding (NutrientValue (Sum v) ms) =
--     pairs ("value" .= v <> "members" .= ms)

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
