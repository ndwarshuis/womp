let T = ../../dhall/Types.dhall

let meals =
        [ T.Schedule::{
          , schMeal =
            { mlName = "dunecash"
            , mlIngs =
              [ T.Ingredient::{
                , ingSource = T.IngredientSource.Custom "water"
                , ingMass = 100.0
                }
              ]
            }
          }
        ]
      : List T.Schedule.Type

let custom =
      [ { mapKey = "water"
        , mapValue = T.CustomIngredient::{
          , scDesc = "hydrated oxygen"
          , scRemainder = 1051
          }
        }
      ]

in  T.Plan::{ schedule = meals, customIngredients = custom }
