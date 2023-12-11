{-# LANGUAGE DerivingVia #-}

module Internal.Types.Main where

import Control.Monad.Error.Class
import Control.Monad.Trans.Except
-- import Internal.Types.Dhall
-- import Internal.Types.FDC
import RIO

instance Exception AppException

newtype AppException = AppException [AppError]
  deriving (Show, Semigroup) via [AppError]

type MonadAppError = MonadError AppException

type AppExcept = AppExceptT Identity

type AppExceptT = ExceptT AppException

data AppError
  = DatePatternError !Natural !Natural !(Maybe Natural) !PatternSuberr
  deriving (Show)

data PatternSuberr = ZeroLength | ZeroRepeats deriving (Show)
