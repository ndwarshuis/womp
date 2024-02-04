let T = ../../dhall/Types.dhall

let custom =
      [ { mapKey = "vitaminD"
        , mapValue = T.CustomIngredient::{
          , scDesc = "vitamin D supplement"
          , scRemainder = 1113
          }
        }
      ]

let yogurt =
      T.Schedule::{
      , schMeal =
        { mlName = "fruit yogurt"
        , mlIngs =
          [ T.Ingredient::{
            , ingSource =
                {- banana -}
                T.IngredientSource.FDC 1105073
            , ingMass = 120.0
            }
          , T.Ingredient::{
            , ingSource =
                {- greek yogurt -}
                T.IngredientSource.FDC 330137
            , ingMass = 200.0
            }
          , T.Ingredient::{
            , ingSource =
                {- almonds -}
                T.IngredientSource.FDC 2346393
            , ingMass = 60.0
            }
          , T.Ingredient::{
            , ingSource =
                {- strawberries -}
                T.IngredientSource.FDC 2346409
            , ingMass = 40.0
            }
          , T.Ingredient::{
            , ingSource =
                {- blueberries -}
                T.IngredientSource.FDC 2346411
            , ingMass = 50.0
            }
          , T.Ingredient::{
            , ingSource = T.IngredientSource.Custom "vitaminD"
            , ingMass = 0.000075
            }
          ]
        }
      , schWhen = T.Cron::{
        , cronWeekly = Some
            ( T.WeekdayPat.OnDays
                [ T.Weekday.Monday, T.Weekday.Wednesday, T.Weekday.Friday ]
            )
        }
      }

in  T.Plan::{ schedule = [ yogurt ], customIngredients = custom }
