{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Types.Dhall where

import Data.Aeson
import Dhall.Marshal.Decode
import Dhall.TH
import GHC.Err
import Internal.Types.TH
import RIO
import RIO.Partial (toEnum)

makeHaskellTypesWith
  (defaultGenerateOptions {generateToDhallInstance = True, generateFromDhallInstance = True})
  [ MultipleConstructors "Weekday" "(./dhall/Types.dhall).Weekday"
  , MultipleConstructors "WeekdayPat" "(./dhall/Types.dhall).WeekdayPat"
  , MultipleConstructors "MDYPat" "(./dhall/Types.dhall).MDYPat"
  , MultipleConstructors "IngredientSource" "(./dhall/Types.dhall).IngredientSource"
  , SingleConstructor "CalorieConversion" "CalorieConversion" "(./dhall/Types.dhall).CalorieConversion.Type"
  , SingleConstructor "Btw" "Btw" "(./dhall/Types.dhall).Btw"
  , SingleConstructor "CustomNutrient" "CustomNutrient" "(./dhall/Types.dhall).CustomNutrient"
  , SingleConstructor "CustomIngredient" "CustomIngredient" "(./dhall/Types.dhall).CustomIngredient.Type"
  , SingleConstructor "RepeatPat" "RepeatPat" "(./dhall/Types.dhall).RepeatPat"
  , SingleConstructor "Modification" "Modification" "(./dhall/Types.dhall).Modification"
  , SingleConstructor "Ingredient" "Ingredient" "(./dhall/Types.dhall).Ingredient.Type"
  , SingleConstructor "Meal" "Meal" "(./dhall/Types.dhall).Meal"
  , SingleConstructor "Cron" "Cron" "(./dhall/Types.dhall).Cron.Type"
  , SingleConstructor "Schedule" "Schedule" "(./dhall/Types.dhall).Schedule.Type"
  ]

deriveProduct
  ["Eq", "Show", "FromJSON"]
  [ "Weekday"
  , "IngredientSource"
  , "CustomIngredient"
  , "CustomNutrient"
  , "WeekdayPat"
  , "RepeatPat"
  , "Btw"
  , "Modification"
  , "Ingredient"
  , "Meal"
  , "MDYPat"
  , "Cron"
  , "Schedule"
  , "CalorieConversion"
  ]

type CustomMap = Map Text CustomIngredient

data Plan = Plan
  { schedule :: [Schedule]
  , customIngredients :: CustomMap
  }
  deriving (Eq, Show, Generic, FromDhall, FromJSON)

-- redefine this manually here since dhall will alphabetize the order
instance Enum Weekday where
  toEnum 0 = Sunday
  toEnum 1 = Monday
  toEnum 2 = Tuesday
  toEnum 3 = Wednesday
  toEnum 4 = Thursday
  toEnum 5 = Friday
  toEnum 6 = Saturday
  toEnum _ = errorWithoutStackTrace "Internal.Types.Dhall.Weekday.toEnum: bad argument"

  fromEnum Sunday = 0
  fromEnum Monday = 1
  fromEnum Tuesday = 2
  fromEnum Wednesday = 3
  fromEnum Thursday = 4
  fromEnum Friday = 5
  fromEnum Saturday = 6
