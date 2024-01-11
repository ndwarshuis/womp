{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Internal.Types.Dhall where

import Data.Aeson
import Dhall.TH
import Internal.Types.TH
import RIO

makeHaskellTypesWith
  (defaultGenerateOptions {generateToDhallInstance = True, generateFromDhallInstance = True})
  [ MultipleConstructors "Weekday" "(./dhall/Types.dhall).Weekday"
  , MultipleConstructors "WeekdayPat" "(./dhall/Types.dhall).WeekdayPat"
  , MultipleConstructors "MDYPat" "(./dhall/Types.dhall).MDYPat"
  , SingleConstructor "Btw" "Btw" "(./dhall/Types.dhall).Btw"
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
  , "WeekdayPat"
  , "RepeatPat"
  , "Btw"
  , "Modification"
  , "Ingredient"
  , "Meal"
  , "MDYPat"
  , "Cron"
  , "Schedule"
  ]

deriving instance Enum Weekday
