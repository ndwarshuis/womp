let T = ../../dhall/Types.dhall

let meals =
        [ T.Schedule::{
          , schMeal =
            { mlName = "fruit yogurt"
            , mlIngs =
              [ T.Ingredient::{
                , ingSource =
                    {- banana -}
                    T.IngredientSource.FDC 1105073
                , ingMass = 150.0
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
                , ingMass = 80.0
                }
              , T.Ingredient::{
                , ingSource =
                    {- blueberries -}
                    T.IngredientSource.FDC 2346411
                , ingMass = 100.0
                }
              ]
            }
          }
        ]
      : List T.Schedule.Type

in  T.Plan::{ schedule = meals }
