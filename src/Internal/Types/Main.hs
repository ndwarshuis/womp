{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
import qualified Data.Csv as C
import Data.Scientific
import RIO
import qualified RIO.Text as T

data RowNutrient = RowNutrient
  { rnId :: Int
  , rnMealName :: T.Text
  , rnDesc :: T.Text
  , rnNutrientName :: Maybe T.Text
  , rnNutrientId :: Maybe Int
  , rnDerivation :: Maybe T.Text
  , rnAmount :: Maybe Scientific
  , rnUnit :: Maybe T.Text
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
  { unitName :: UnitName
  , unitBase :: Prefix
  }
  deriving (Show, Eq)

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
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)
