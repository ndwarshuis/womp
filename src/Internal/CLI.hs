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
          <> progDesc "Plan and track your macro/micronutrients"
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
        <> help "Be obnoxious (multiple times to escalate)"

subcommand :: Parser SubCommand
subcommand =
  hsubparser
    ( command
        "table"
        ( info
            (ExportTabular <$> tabular)
            (progDesc "Export meal plan in tabular format")
        )
        <> command
          "tree"
          ( info
              (ExportTree <$> tree)
              (progDesc "Export meal plan in tree format")
          )
        <> command
          "summary"
          ( info
              (Summarize <$> summarize)
              (progDesc "Print table of meals and their ingredients")
          )
        <> command
          "nutrients"
          ( info
              (pure ListNutrients)
              (progDesc "List nutrients available for parsing")
          )
        <> command
          "fetch"
          ( info
              (Fetch <$> fetchDump)
              (progDesc "Fetch a food by ID")
          )
        <> command
          "dump"
          ( info
              (Dump <$> fetchDump)
              (progDesc "Dump JSON for food by ID")
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
          <> help "Path to config with schedules and meals"
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
                [ "Number of threads for processing ingredients."
                , "Anything less than 1 will use all available cores"
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
          <> help "Number of digits after decimal to keep"
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
          <> help "Display unknown nutrients in output"
      )
    <*> optional
      ( strOption
          ( long "prefix"
              <> short 'p'
              <> help "Force all masses to this prefix (energy will remain in kcal)"
          )
      )
    <*> strOption
      ( long "filter"
          <> short 'F'
          <> metavar "FILTER"
          <> help filterHelp
          <> value ""
      )
    <*> switch
      ( long "noenergy"
          <> short 'E'
          <> help "Don't show energy in output"
      )
    <*> strOption
      ( long "annotations"
          <> short 'A'
          <> metavar "ANNOTATIONS"
          <> help "Comma-separated list of annotations to show"
          <> value ""
      )
  where
    -- TODO fix this once I figure out what I'm really doing
    filterHelp =
      unwords
        [ "Comma separated list of filter keys. Valid options are"
        , "'meal~<regexp>', 'ingredient~<regexp>', 'nutrient~<regexp>',"
        , "or 'value<op><value>[<prefix>]'. In the case of the first three,"
        , "<regexp> is a regular expression against which the specified string"
        , "will be matched. For value, <op> is one of '<', '>', '>=', '<=',"
        , "or '=', <value> is a whole or decimal number, and <prefix> is an"
        , "optional SI prefix such as 'G' or 'M' to which <value> will be"
        , "scaled. If 'nutrient' or 'value' queries match a given node in the"
        , "nutrient tree, everthing except that node and its children are"
        , "discarded. Invert a filter by prefixing with '!'."
        ]

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
        [ "Comma separated list of sort keys. Valid keys are"
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
        <> help "Include header above the table"
    )

grouping :: Parser GroupOptions
grouping =
  GroupOptions
    <$> switch (long "date" <> short 'D' <> help "Group by date range")
    <*> switch (long "meal" <> short 'M' <> help "Group by meal")
    <*> switch (long "ingredient" <> short 'G' <> help "Group by ingredient")

tree :: Parser TreeExportOptions
tree =
  TreeExportOptions
    <$> displayOptions
    <*> switch
      ( long "json"
          <> short 'j'
          <> help "Output JSON instead of YAML"
      )
    <*> common

force :: Parser Bool
force =
  switch
    ( long "force"
        <> short 'f'
        <> help "Download ingredients even if they are already cached"
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
          <> help daysHelp
          <> value 7
      )
    <*> optional
      ( option
          auto
          ( long "interval"
              <> short 'I'
              <> metavar "DAYS"
              <> help intervalHelp
          )
      )
    <*> option
      auto
      ( long "normalize"
          <> short 'N'
          <> metavar "NORMALIZE"
          <> help normalizeHelp
          <> value 1
      )
  where
    daysHelp =
      unwords
        [ "Length of interval in days within which summary will"
        , "be calculated (ignored if END is present)"
        ]
    intervalHelp =
      unwords
        [ "Aggregate in intervals of this length throughout the time denoted"
        , "by --start and --end/--days"
        ]
    normalizeHelp =
      unwords
        [ "Normalize all values to this (for instance to put week-long"
        , "schedule on per/day basis)"
        ]

displayOptions :: Parser TreeDisplayOptions
displayOptions =
  TreeDisplayOptions
    <$> switch
      ( long "expandUnits"
          <> short 'x'
          <> help "Show prefix and base unit as separate keys"
      )

startDay :: Parser (Maybe Day)
startDay =
  parseDay
    "start"
    's'
    "Start date on which to begin summary calculations"

endDay :: Parser (Maybe Day)
endDay =
  parseDay
    "end"
    'e'
    "End date on which to stop summary calculations (exclusive)"

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
            <> help h
        )
    )
  where
    h =
      unwords
        [ "API key for USDA FoodData Central."
        , "This will be cached so it only needs to be provided once unless changed"
        ]
