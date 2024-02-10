{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Data.Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Csv as C
import qualified Data.Map.Merge.Strict as MMS
import Data.Monoid
import Data.Scientific
import GHC.Generics
import Internal.Types.CLI
import Internal.Types.Dhall
import Internal.Types.FoodItem
import RIO
import qualified RIO.Map as M
import qualified RIO.Text as T
import RIO.Time

--------------------------------------------------------------------------------
-- CLI (repackaged)

data AllTreeDisplayOptions = AllTreeDisplayOptions
  { atdoOpts :: !TreeDisplayOptions
  , atdoShowUnknowns :: !Bool
  , atdoUnits :: !(Maybe Prefix)
  , atdoRoundDigits :: !Int
  , atdoFilter :: ![FilterKey]
  }

data AllTabularDisplayOptions = AllTabularDisplayOptions
  { atabShowUnknowns :: !Bool
  , atabUnits :: !(Maybe Prefix)
  , atabRoundDigits :: !Int
  , atabSort :: ![SortKey]
  , atabFilter :: ![FilterKey]
  }

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

data FilterKey = FilterKey Bool FilterData
  deriving (Eq, Show)

data FilterData
  = FilterMeal !Text
  | FilterIngredient !Text
  | FilterNutrient !Text
  | FilterValue !Mass !Operator
  deriving (Eq, Show)

data Operator = EQ_ | LT_ | GT_ | LTE_ | GTE_
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Nutrient tree
--
-- Built-in structure that represents the hierarchy of nutrients to measure.

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

type Branches = NonEmpty (NutrientChoice Node)

data Node
  = MeasuredHeader MeasuredNutrient NutTree
  | UnmeasuredHeader SummedNutrient Branches
  | Leaf MeasuredNutrient

data NutrientChoice a = NutrientSingle a | NutrientMany (NonEmpty a)

-- | Nutrient that should be directly measured. If it is not measured, it will
-- be summed from the value of its children if defined.
data MeasuredNutrient
  = Direct DirectNutrient
  | Alternate AltNutrient
  deriving (Show, Eq, Ord)

-- | Single nutrient ID to be directly measured
data DirectNutrient = DirectNutrient
  { mnId :: NID
  , mnName :: Text
  , mnDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

-- | Multiple nutrient IDs to be directly measured. Used for cases like protein
-- and nitrogen, where some provide one or the other and they differ by a ratio.
data AltNutrient = AltNutrient
  { anName :: Text
  , anDisplayPrefix :: Prefix
  , anChoices :: NonEmpty (NID, Maybe Scientific)
  }
  deriving (Show, Eq, Ord)

-- | Nutrient which is to be summed from other directly measured nutrients
data SummedNutrient = SummedNutrient
  { snName :: Text
  , snDisplayPrefix :: Prefix
  }
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Intermediate Nutrient Tree
--
-- An intermediate tree structure which makes traversing FoodItem's using the
-- nutrient tree efficient and clear. Specifically, this makes a distinction b/t
-- "Quantified" and "Unquantified" nodes, which are those that do and don't have
-- measured values associated with them respectively.

data DisplayNutrient = DisplayNutrient {dnName :: Text, dnPrefix :: Prefix}
  deriving (Show, Eq, Ord)

-- | Node with a value associated with it.
-- It may no children (in which case it is a nutrient leaf) or many children,
-- which many be a mixture of known and unknown.
data QuantifiedNode = QuantifiedNode
  { fnValue :: Mass
  -- ^ Mass of this node
  , fnNut :: DisplayNutrient
  -- ^ Nutrient identifier associated with this node
  , fnKnown :: [ParsedTreeNode]
  -- ^ Subnutrients underneath this node with known mass
  , fnUnknown :: Either [UnknownTree] QuantifiedNode
  -- ^ Subnutrients underneath this node with no known individual masses but
  -- known collective masses. This mass and all those under the "known" field
  -- must sum to that of the "value" field
  }

-- | Node with no value associated with it, which means a) it must have at
-- least one child, b) its value must be summed from its child(ren) and c)
-- it cannot have unknown children.
data UnquantifiedNode = UnquantifiedNode
  { pnNut :: DisplayNutrient
  -- ^ Nutrient associated with this node
  , pnKnown :: NonEmpty ParsedTreeNode
  -- ^ Subnutrients underneath this node with known mass
  }

data ParsedTreeNode = Quantified QuantifiedNode | Unquantified UnquantifiedNode

data UnknownTree = UnknownTree Text [UnknownTree]
  deriving (Eq, Ord, Show)

instance ToJSON UnknownTree where
  toJSON (UnknownTree n ts) = object ["name" .= n, "children" .= ts]

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

data DisplayNode a = DisplayNode
  { dnValue :: a
  , dnKnown :: DisplayMap a
  , dnUnknown :: Map [UnknownTree] a
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
      merge_ :: (Ord k, Semigroup v) => Map k v -> Map k v -> Map k v
      merge_ =
        MMS.merge
          MMS.preserveMissing
          MMS.preserveMissing
          (MMS.zipWithMatched (\_ x y -> x <> y))

data DisplayTree_ g a b = DisplayTree_
  { dtMap :: DisplayMap a
  , dtEnergy :: b
  , dtGroup :: g
  }
  deriving (Generic, Show)

type DisplayMap a = Map DisplayNutrient (DisplayNode a)

-- fmap cheat code: make mass and energy polymorphic so I don't need to use
-- a lens to "map" over this structure
instance Bifunctor (DisplayTree_ g) where
  bimap f g r@(DisplayTree_ as b _) = r {dtMap = fmap f <$> as, dtEnergy = g b}

type DisplayTreeSum = DisplayTree_ () (Sum Mass) (Sum Energy)

-- only allow "adding" together if there is no grouping data to clobber
instance Semigroup (DisplayTree_ () (Sum Mass) (Sum Energy)) where
  (<>) a b = DisplayTree_ (M.unionWith (<>) (dtMap a) (dtMap b)) (dtEnergy a + dtEnergy b) ()

type DisplayTree g = DisplayTree_ g Mass Energy

--------------------------------------------------------------------------------
-- display rows

-- Types for exporting nutrient data in a csv-like format. This is analogous
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

--------------------------------------------------------------------------------
-- Summary rows
--
-- For displaying the summary in csv-like format

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

--------------------------------------------------------------------------------
-- Nutrient Tree Rows
--
-- For displaying the available nutrients in a table

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

--------------------------------------------------------------------------------
-- Application-wide errors
--
-- Use one centralized error type for the entire application, which will allow
-- raising errors at any point, potentially multiple at once, and then catching
-- them at the top level

instance Exception AppException

newtype AppException = AppException [AppError]
  deriving (Show, Semigroup) via [AppError]

type MonadAppError = MonadError AppException

data AppError
  = DatePatternError !Natural !Natural !(Maybe Natural) !PatternSuberr
  | DateDaysEndError !Int
  | DaySpanError
  | IntervalError !Int
  | JSONError !ByteString
  | EmptyMeal !T.Text
  | MissingAPIKey !FilePath
  | FileTypeError !FilePath
  | CustomIngError !CustomIngError
  | MissingCustom !Text
  | MassError !IngredientSource !Double
  | SortKeys !Text
  | FilterKeys !Text
  | EmptySchedule !Bool
  | NormalizeError !Int
  | PrefixError !Text
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)

data CustomIngError
  = CustomDups Text (NonEmpty NID)
  | TooMuchMass Text
  deriving (Show)

--------------------------------------------------------------------------------
-- Misc

type DaySpan = (Day, Int)

newtype Energy = Energy {unEnergy :: Scientific}
  deriving (Show, Eq, Ord, Num, ToJSON, Fractional, Real, RealFrac) via Scientific
