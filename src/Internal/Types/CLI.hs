{-# LANGUAGE DerivingVia #-}

module Internal.Types.CLI where

import Internal.Types.FoodItem
import RIO
import RIO.Time

data CLIOptions = CLIOptions CommonOptions SubCommand

data SubCommand
  = Fetch !FetchDumpOptions
  | Dump !FetchDumpOptions
  | ExportTabular !TabularExportOptions
  | ExportTree !TreeExportOptions
  | ListNutrients
  | Summarize !SummarizeOptions

newtype CommonOptions = CommonOptions
  { coVerbosity :: Int
  }

data FetchDumpOptions = FetchDumpOptions
  { foID :: !FID
  , foForce :: !Bool
  , foKey :: !(Maybe APIKey)
  }

newtype APIKey = APIKey {unAPIKey :: Text} deriving (IsString) via Text

data CommonExportOptions = CommonExportOptions
  { ceoMealplan :: !MealplanOptions
  , ceoGroup :: !GroupOptions
  , ceoShowUnknowns :: !Bool
  , ceoPrefix :: !(Maybe Text)
  }

data TabularExportOptions = TabularExportOptions
  { tabCommonExport :: !CommonExportOptions
  , tabSort :: !Text
  , tabHeader :: !Bool
  }

data SummarizeOptions = SummarizeOptions
  { soMealplanOptions :: !MealplanOptions
  , soHeader :: !Bool
  }

data TreeExportOptions = TreeExportOptions
  { treeDisplay :: !TreeDisplayOptions
  , treeJSON :: !Bool
  , treeCommonExport :: !CommonExportOptions
  }

data MealplanOptions = MealplanOptions
  { moMealPath :: !FilePath
  , moDateInterval :: !DateIntervalOptions
  , moForce :: !Bool
  , moThreads :: !Int
  , moKey :: !(Maybe APIKey)
  , moRoundDigits :: !Int
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
