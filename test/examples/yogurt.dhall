let T = ../../dhall/Types.dhall

in    [ T.Schedule::{
        , schMeal =
          { mlName = "fruit yogurt"
          , mlIngs =
            [ T.Ingredient::{
              , ingFID =
                  {- banana -}
                  1105073
              , ingMass = 150.0
              }
            , T.Ingredient::{
              , ingFID =
                  {- greek yogurt -}
                  330137
              , ingMass = 200.0
              }
            , T.Ingredient::{
              , ingFID =
                  {- almonds -}
                  2346393
              , ingMass = 60.0
              }
            , T.Ingredient::{
              , ingFID =
                  {- strawberries -}
                  2346409
              , ingMass = 80.0
              }
            , T.Ingredient::{
              , ingFID =
                  {- blueberries -}
                  2346411
              , ingMass = 100.0
              }
            ]
          }
        }
      ]
    : List T.Schedule.Type
