let Map =
      https://prelude.dhall-lang.org/v23.0.0/Map/Type
        sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let Modification = { modNutID : Natural, modScale : Double }

let Ingredient =
      { Type =
          { ingFID : Natural
          , ingMass : Double
          , ingModifications : List Modification
          }
      , default.ingModifications = [] : List Modification
      }

let Meal = { mlName : Text, mlIngs : List Ingredient.Type }

let RepeatPat =
    {-
    Means to match a repeated set of numeric values.
    -}
      { rpStart :
          {-
          Initial value to match
          -}
          Natural
      , rpBy :
          {-
          Distance between each repeated value
          -}
          Natural
      , rpRepeats :
          {-
          Number of repeats after initial value to match. If not given, this
          number continues until an upper bound determined from context.
          -}
          Optional Natural
      }

let Btw = { btStart : Natural, btEnd : Natural }

let MDYPat =
    {-
    Means to match either a year, month, or day in a date (the matched component
    is determined by context)

    Single: match a single number
    Multi: match several numbers
    Repeat: match a repeated pattern
    After: match any number greater than a given value
    Before: match any number less than a given value
    Between: match any number between two values
    -}
      < Single : Natural
      | Multi : List Natural
      | Repeat : RepeatPat
      | After : Natural
      | Before : Natural
      | Between : Btw
      >

let Weekday =
      < Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday >

let WeekdayPat =
    {-
    Means to match a given day of week

    OnDay: Match a single weekday
    OnDays: Match multiple weekdays
    -}
      < OnDay : Weekday | OnDays : List Weekday >

let Cron =
    {-
    Means of matching dates according to their component parts.

    This is similar to 'cron' patterns in unix-like systems.
    -}
      { Type =
          { cronWeekly : Optional WeekdayPat
          , cronYear : Optional MDYPat
          , cronMonth : Optional MDYPat
          , cronDay : Optional MDYPat
          }
      , default =
        { cronWeekly = None WeekdayPat
        , cronYear = None MDYPat
        , cronMonth = None MDYPat
        , cronDay = None MDYPat
        }
      }

let Schedule =
      { Type =
          { schMeal : Meal, schWhen : Cron.Type, schScale : Optional Double }
      , default = { schWhen = Cron::{=}, schScale = Some 1.0 }
      }

let EquivalenceGroup = Map Text (List Natural)

let Config =
      { Type =
          { schedule : Schedule.Type
          , equivalenceGroups : EquivalenceGroup
          , exclusions : List Natural
          }
      , default =
        { equivalenceGroups =
          [ { mapKey = "Energy", mapValue = [ 2047, 2048 ] } ]
        , exclusions = [ 1062 ]
        }
      }

in  { Modification
    , Btw
    , Ingredient
    , Meal
    , RepeatPat
    , MDYPat
    , Weekday
    , WeekdayPat
    , Cron
    , Schedule
    , Config
    }
