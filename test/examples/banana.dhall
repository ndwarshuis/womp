let T = ../../dhall/Types.dhall

in    [ T.Schedule::{
        , schMeal =
          { mlName = "jumbo banana"
          , mlIngs = [ T.Ingredient::{ ingFID = 1105073, ingMass = 150.0 } ]
          }
        , schWhen = T.Cron::{
          , cronWeekly = Some (T.WeekdayPat.OnDay T.Weekday.Sunday)
          }
        }
      ]
    : List T.Schedule.Type
