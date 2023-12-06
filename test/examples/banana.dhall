let T = ../../dhall/Types.dhall

in    [ T.Schedule::{
        , schMeal =
          { mlName = "jumbo banana"
          , mlIngs = [ T.Ingredient::{ ingFID = 1105073, ingMass = 150.0 } ]
          }
        }
      ]
    : List T.Schedule.Type
