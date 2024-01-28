let Map =
      https://prelude.dhall-lang.org/v23.0.0/Map/Type
        sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let Prefix =
      < Nano
      | Micro
      | Milli
      | Centi
      | Deci
      | Unity
      | Deca
      | Hecto
      | Kilo
      | Mega
      | Giga
      >

let Modification = { modNutID : Natural, modScale : Double }

let CustomNutrient = { cnID : Natural, cnMass : Double, cnPrefix : Prefix }

let CalorieConversion =
      { Type = { ccFat : Double, ccProtein : Double, ccCarbs : Double }
      , default = { ccFat = 9.0, ccCarbs = 4.0, ccProtein = 4.0 }
      }

let CustomIngredient =
      { Type =
          { scDesc : Text
          , scRemainder : Natural
          , scRemainderPrefix : Prefix
          , scNutrients : List CustomNutrient
          , scCalorie : CalorieConversion.Type
          , scProtein : Double
          }
      , default =
        { scCalorie = CalorieConversion::{=}
        , scProtein = 6.25
        , scNutrients = [] : List CustomNutrient
        }
      }

let IngredientSource = < Custom : Text | FDC : Natural >

let Ingredient =
      { Type =
          { ingSource : IngredientSource
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

let Config =
      { Type =
          { schedule : Schedule.Type
          , customIngredients : Map Text CustomIngredient.Type
          }
      , default.customIngredients = [] : Map Text CustomIngredient.Type
      }

in  { Modification
    , Btw
    , Prefix
    , CustomNutrient
    , CustomIngredient
    , IngredientSource
    , Ingredient
    , Meal
    , RepeatPat
    , MDYPat
    , Weekday
    , WeekdayPat
    , Cron
    , Schedule
    , Config
    , CalorieConversion
    }
