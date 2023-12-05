{-# LANGUAGE DeriveAnyClass #-}

module Internal.Types.FDC where

import Data.Aeson
import RIO
import qualified RIO.Text as T

data AbridgedFoodItem = AbridgedFoodItem
  { dataType :: T.Text
  , description :: T.Text
  , fdcId :: Int
  , pulicationDate :: T.Text
  }
  deriving (Show, Generic, FromJSON)
