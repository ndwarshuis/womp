{-
Example for a meal plan with an exotic fruit yogurt dish to be consumed on
Mondays, Wednesdays, and Fridays.

Use this as a starting point for more sophisticated meal plans.
-}
let T =
    {-
    Import the types to define a meal plan. This isn't strictly necessary but
    makes things alot easier by providing type-checking and defaults.

    In your config, you would specifically import from the repo directly like:

    let T =
      https://raw.githubusercontent.com/ndwarshuis/womp/master/dhall/Types.dhall
    -}
      ../../dhall/Types.dhall

let custom =
    {-
    Define some custom ingredients for things that are not found in the FDC.

    In this case we define a vitamin D supplement to be added to our yogurt.
    -}
      [ { mapKey
          {-
          ID of this ingredient; this is how we will refer to this ingredient
          elsewhere in the config
          -}
          = "vitaminD"
        , mapValue = T.CustomIngredient::{
          , scDesc = "vitamin D supplement"
          , scRemainder
            {-
            This is the nutrient ID for 'Vitamin D3 (calcifediol)' which can
            be identified by running `womp nutrients`.
            -}
            = 1113
          }
        }
      ]

let yogurt_meal =
    {-
    Definition of our yogurt meal
    -}
      { mlName
        {-
        Name of the meal which will be displayed under the 'meal' column/key
        when applicable.
        -}
        = "fruit yogurt"
      , mlIngs
        {-
        List of ingredients comprising our yogurt.

        Most of these are straight from the FDC website
        (https://fdc.nal.usda.gov/fdc-app.html#/) and can be found by searching
        for the relevant keywords and copying the "FDC ID" number within the
        desired food item. These are referred to by 'IngredientSource.FDC'.

        Our custom vitamin D supplement is exemplified near the bottom which
        uses 'IngredientSource.Custom' to refer to the textual key we defined
        above.

        All masses are in grams.
        -}
        =
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

let yogurt =
    {-
    Define when our yogurt is to be eaten.

    The timing is defined by a cron-like type which can include year, month,
    and day matching in addition to weekdays as shown below. Learn more about
    this type in dhall/Types.dhall from the root of this repository.
    -}
      T.Schedule::{
      , schMeal = yogurt_meal
      , schWhen = T.Cron::{
        , cronWeekly = Some
            ( T.WeekdayPat.OnDays
                [ T.Weekday.Monday, T.Weekday.Wednesday, T.Weekday.Friday ]
            )
        }
      }

in  T.Plan::{
    , schedule
      {-
      We could have multiple meals scheduled at different times if we wanted.
      -}
      =
      [ yogurt ]
    , customIngredients
      {-
      Put the custom ingredients here so that 'womp' can be made aware of them.
      -}
      = custom
    }
