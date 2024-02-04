module Internal.CLI (parseCLI) where

import Internal.Types.CLI
import Options.Applicative
import RIO hiding (force)
import RIO.Time

parseCLI :: IO CLIOptions
parseCLI =
  execParser $
    info
      (options <**> helper)
      ( fullDesc
          <> header "womp: what's on my plate"
          <> progDesc "plan and track your macro/micronutrients"
      )

options :: Parser CLIOptions
options = CLIOptions <$> commonOptions <*> subcommand

commonOptions :: Parser CommonOptions
commonOptions =
  CommonOptions
    <$> (length <$> many (flag' () c))
  where
    c =
      long "verbose"
        <> short 'v'
        <> help "be obnoxious (multiple times to escalate)"

subcommand :: Parser SubCommand
subcommand =
  hsubparser
    ( command
        "table"
        ( info
            (ExportTabular <$> tabular)
            (progDesc "export meal plan in tabular format")
        )
        <> command
          "tree"
          ( info
              (ExportTree <$> tree)
              (progDesc "export meal plan in tree format")
          )
        <> command
          "summary"
          ( info
              (Summarize <$> summarize)
              (progDesc "print table of meals and their ingredients")
          )
        <> command
          "nutrients"
          ( info
              (pure ListNutrients)
              (progDesc "list nutrients available for parsing")
          )
        <> command
          "fetch"
          ( info
              (Fetch <$> fetchDump)
              (progDesc "fetch a food by ID")
          )
        <> command
          "dump"
          ( info
              (Dump <$> fetchDump)
              (progDesc "dump JSON for food by ID")
          )
    )

fetchDump :: Parser FetchDumpOptions
fetchDump =
  FetchDumpOptions
    <$> option
      auto
      ( long "fid"
          <> short 'i'
          <> metavar "FOODID"
          <> help "ID for the food to pull from the database"
      )
    <*> force
    <*> apikey

mealplan :: Parser MealplanOptions
mealplan =
  MealplanOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "PATH"
          <> help "path to config with schedules and meals"
      )
    <*> dateInterval
    <*> force
    <*> option
      auto
      ( long "threads"
          <> short 't'
          <> metavar "NUM"
          <> help
            ( unwords
                [ "number of threads for processing ingredients;"
                , "anything less than 1 will use all available cores"
                ]
            )
          <> value 2
      )
    <*> apikey
    <*> option
      auto
      ( long "round"
          <> short 'r'
          <> metavar "DIGITS"
          <> help "number of digits after decimal to keep"
          <> value 3
      )

summarize :: Parser SummarizeOptions
summarize =
  SummarizeOptions
    <$> mealplan
    <*> headerTab

common :: Parser CommonExportOptions
common =
  CommonExportOptions
    <$> mealplan
    <*> grouping
    <*> switch
      ( long "unknowns"
          <> short 'u'
          <> help "display unknown nutrients in output"
      )
    <*> optional
      ( strOption
          ( long "prefix"
              <> short 'p'
              <> help "force all masses to this prefix (energy will remain in kcal)"
          )
      )

tabular :: Parser TabularExportOptions
tabular =
  TabularExportOptions
    <$> common
    <*> strOption
      ( long "sort"
          <> short 'S'
          <> metavar "KEYLIST"
          <> help sortHelp
          <> value ""
      )
    <*> headerTab
  where
    sortHelp =
      unwords
        [ "comma separated list of sort keys; valid keys are"
        , "'date', 'meal', 'ingredient', 'nutrient', 'parent'"
        , "and 'value' which correspond to the headers to be sorted;"
        , "each must be prefixed with '+' or '-' for ascending or"
        , "descending respectively"
        ]

headerTab :: Parser Bool
headerTab =
  switch
    ( long "header"
        <> short 'H'
        <> help "include header above the table"
    )

grouping :: Parser GroupOptions
grouping =
  GroupOptions
    <$> switch (long "date" <> short 'D' <> help "group by date range")
    <*> switch (long "meal" <> short 'M' <> help "group by meal")
    <*> switch (long "ingredient" <> short 'G' <> help "group by ingredient")

tree :: Parser TreeExportOptions
tree =
  TreeExportOptions
    <$> displayOptions
    <*> switch
      ( long "json"
          <> short 'j'
          <> help "output JSON instead of YAML"
      )
    <*> common

force :: Parser Bool
force =
  switch
    ( long "force"
        <> short 'f'
        <> help "download ingredients even if they are already cached"
    )

dateInterval :: Parser DateIntervalOptions
dateInterval =
  DateIntervalOptions
    <$> startDay
    <*> endDay
    <*> option
      auto
      ( long "days"
          <> short 'd'
          <> metavar "DAYS"
          <> help "length of interval in days within which summary will be calculated (ignored if END is present)"
          <> value 7
      )
    <*> optional
      ( option
          auto
          ( long "interval"
              <> short 'I'
              <> metavar "DAYS"
              <> help "aggregate in intervals of this length throughout the time denoted by --start and --end/--days"
          )
      )
    <*> option
      auto
      ( long "normalize"
          <> short 'N'
          <> metavar "NORMALIZE"
          <> help "normalize all values to this (for instance to put week-long schedule on per/day basis)"
          <> value 1
      )

displayOptions :: Parser TreeDisplayOptions
displayOptions =
  TreeDisplayOptions
    <$> switch
      ( long "expandUnits"
          <> short 'x'
          <> help "show prefix and base unit as separate keys"
      )

startDay :: Parser (Maybe Day)
startDay =
  parseDay
    "start"
    's'
    "start date on which to begin summary calculations"

endDay :: Parser (Maybe Day)
endDay =
  parseDay
    "end"
    'e'
    "end date on which to stop summary calculations (exclusive)"

parseDay :: String -> Char -> String -> Parser (Maybe Day)
parseDay l s d =
  fmap readDay
    <$> optional
      ( strOption
          ( long l
              <> short s
              <> metavar "YYYY-MM-DD"
              <> help d
          )
      )

readDay :: String -> Day
readDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

apikey :: Parser (Maybe APIKey)
apikey =
  optional
    ( strOption
        ( long "apikey"
            <> short 'k'
            <> metavar "APIKEY"
            <> help "API key for USDA FoodData Central"
        )
    )
