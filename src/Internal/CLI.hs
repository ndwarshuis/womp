module Internal.CLI (parseCLI) where

import Data.Char (toUpper)
import Internal.Types.Main
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
    <$> optional
      ( strOption
          ( long "apikey"
              <> short 'k'
              <> metavar "APIKEY"
              <> help "API key for USDA FoodData Central"
          )
      )
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "be obnoxious"
      )

subcommand :: Parser SubCommand
subcommand =
  subparser
    ( command
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
        <> command
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

tabular :: Parser TabularOptions
tabular =
  TabularOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> help "path to config with schedules and meals"
      )
    <*> dateInterval
    <*> force
    <*> option
      auto
      ( long "threads"
          <> short 't'
          <> metavar "THREADS"
          <> help "number of threads for processing ingredients"
          <> value 2
      )
    <*> grouping

grouping :: Parser GroupOptions
grouping =
  GroupOptions
    <$> switch (long "date" <> short 'D' <> help "group by date range")
    <*> switch (long "meal" <> short 'M' <> help "group by meal")
    <*> switch (long "ingredient" <> short 'I' <> help "group by ingredient")

tree :: Parser TreeOptions
tree =
  TreeOptions
    <$> displayOptions
    <*> switch
      ( long "json"
          <> short 'j'
          <> help "summarize output in JSON (display options are ignored)"
      )
    <*> tabular

force :: Parser Bool
force = switch (long "force" <> short 'f' <> help "force retrieve")

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
              <> metavar "INTERVAL"
              <> help "length of time (in days) to aggregate summary"
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

displayOptions :: Parser DisplayOptions
displayOptions =
  DisplayOptions
    <$> switch
      ( long "unknowns"
          <> short 'u'
          <> help "display unknown nutrients in output"
      )
    <*> switch
      ( long "expandUnits"
          <> short 'x'
          <> help "show prefix and base unit as separate keys (JSON only)"
      )
    <*> switch
      ( long "unityUnits"
          <> short 'U'
          <> help "show all masses in grams (no prefix)"
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
              <> metavar (fmap toUpper l)
              <> help d
          )
      )

readDay :: String -> Day
readDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"
