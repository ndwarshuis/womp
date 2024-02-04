{-
Meal plan configuration

Terminology
* nutrient: A measurable substance which has some biological consequence
  (ie 'Glucose')
* ingredient: A collection of nutrients, all of which are specified by mass
  relative to a 100g reference standard.
* meal: a collection of ingredients with a name (aka a "recipe")
* schedule: defines the times at which a meal is consumed
* plan: a collection of schedules
-}
let Map =
      https://prelude.dhall-lang.org/v23.0.0/Map/Type
        sha256:210c7a9eba71efbb0f7a66b3dcf8b9d3976ffc2bc0e907aadfb6aa29c333e8ed

let Modification =
    {-
    Modification for an ingredient
    -}
      { modNutID
        {-
        Nutrient to modify as specified by an ID

        Use 'womp nutrients' to print a list of all available IDs
        -}
        : Natural
      , modScale
        {-
        Degree to which the nutrient should be modified.

        For now this is just a simple multiplier (ie 1.0 will do nothing).
        Must be greater than 0.

        In the future this may include more sophisticated techniques.
        -}
        : Double
      }

let CustomNutrient =
    {-
    Nutrient for use in a custom ingredient.

    These aren't actually "custom" in the sense that they can be totally made
    up; they must have a valid ID as can be found in 'womp nutrients'
    -}
      { cnID
        {-
        ID for this nutrient
        -}
        : Natural
      , cnMass
        {-
        Mass for this nutrient in grams. Must be a positive number.
        -}
        : Double
      }

let CalorieConversion =
    {-
    Conversion factors for use in calculating calories. These all must be
    positive numbers.
    -}
      { Type = { ccFat : Double, ccProtein : Double, ccCarbs : Double }
      , default = { ccFat = 9.0, ccCarbs = 4.0, ccProtein = 4.0 }
      }

let CustomIngredient =
    {-
    A custom ingredient definition.

    These must have at least one nutrient, which is denoted as "remainder".
    Any other nutrients are specified in 'scNutrients'.

    The nutrient content is specified per 100g analogous to the USDA FDC
    database. "remainder" is computed by subtracting the sum of 'scNutrients'
    from 100g. It is an error if this is not positive.

    One can also specify calorie and protein conversions, which almost nobody
    will ever need unless they have a good reason.
    -}
      { Type =
          { scDesc
            {-
            Description/name of this ingredient
            -}
            : Text
          , scRemainder
            {-
            ID for the one required nutrient.

            See valid nutrient IDs with 'womp nutrients'
            -}
            : Natural
          , scNutrients
            {-
            All other nutrients
            -}
            : List CustomNutrient
          , scCalorie
            {-
            Calorie conversion factors.

            This will default to Atwater General Factors unless specified,
            which one probably won't need to do unless they have data.
            -}
            : CalorieConversion.Type
          , scProtein
            {-
            Protein conversion factor (from nitrogen).

            In reality almost nobody will need to use this, since it only makes
            sense if one specifies nitrogen as a nutrient.
            -}
            : Double
          }
      , default =
        { scCalorie = CalorieConversion::{=}
        , scProtein = 6.25
        , scNutrients = [] : List CustomNutrient
        }
      }

let IngredientSource =
    {-
    Source for an ingredient.

    Can either be a custom nutrient (as denoted by a textual key to be found
    in 'customIngredients' below) or an FDC ID.
    -}
      < Custom : Text | FDC : Natural >

let Ingredient =
    {-
    An ingredient for a meal
    -}
      { Type =
          { ingSource
            {-
            Source for this ingredient (either a custom identifier or FDC ID)
            -}
            : IngredientSource
          , ingMass
            {-
            Mass for this ingredient you intent to include in a meal.

            Must be positive.

            NOTE: this is the mass BEFORE modifications are applied. Ie one
            would measure the amount of X added to a recipe prior to the
            processing which would apply the modifications.
            -}
            : Double
          , ingModifications
            {-
            Modifications for this nutrient.

            This is intended to simulate simple processing steps like cooking or
            drying which result in a somewhat predictable change in a few
            ingredients (like water)
            -}
            : List Modification
          }
      , default.ingModifications = [] : List Modification
      }

let Meal =
    {-
    A recipe for a meal
    -}
      { mlName
        {-
        Name of this meal
        -}
        : Text
      , mlIngs
        {-
        Ingredients comprising this meal
        -}
        : List Ingredient.Type
      }

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

let Btw =
    {-
    Matches between two numerical values
    -}
      { btStart : Natural, btEnd : Natural }

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
    {-
    Defines when a meal is consumed.
    -}
      { Type =
          { schMeal
            {-
            Meal to schedule
            -}
            : Meal
          , schWhen
            {-
            When to schedule said meal
            -}
            : Cron.Type
          , schScale
            {-
            Scaling factor for meal (in case one wants to easily make it twice as
            big, small, etc).

            Must be a positive number.
            -}
            : Optional Double
          }
      , default = { schWhen = Cron::{=}, schScale = Some 1.0 }
      }

let Plan =
    {-
    Global meal plan.

    This contains both the schedule and any custom ingredients which may be
    used in the schedule by some meal.
    -}
      { Type =
          { schedule : List Schedule.Type
          , customIngredients : Map Text CustomIngredient.Type
          }
      , default.customIngredients = [] : Map Text CustomIngredient.Type
      }

in  { Modification
    , Btw
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
    , Plan
    , CalorieConversion
    }
