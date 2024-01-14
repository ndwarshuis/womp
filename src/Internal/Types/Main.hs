{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import Data.Aeson
import qualified Data.Csv as C
import qualified Data.Map.Merge.Strict as MMS
import Data.Monoid
import Data.Monoid.Generic
import Data.Scientific
import GHC.Generics
import Internal.Types.Dhall
import Internal.Types.FoodItem
import RIO
import qualified RIO.Map as M
import RIO.State
import qualified RIO.Text as T
import RIO.Time

data CLIOptions = CLIOptions CommonOptions SubCommand

data CommonOptions = CommonOptions
  { coKey :: !(Maybe APIKey)
  , coVerbosity :: !Bool
  }

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

data ValidSchedule a = ValidSchedule
  { vsIngs :: NonEmpty Ingredient
  , vsMeta :: a
  }

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
