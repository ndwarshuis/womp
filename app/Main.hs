module Main (main) where

-- import Lib
import Options.Applicative
import RIO
import qualified RIO.Text as T

main :: IO ()
main = parse =<< execParser o
  where
    o =
        info
            (options <**> helper)
            ( fullDesc
                <> header "carbon: nutrient planner"
                <> progDesc "plan and track your macro/micronutrients"
            )

options :: Parser Options
options = Options <$> commonOptions <*> subcommand

commonOptions :: Parser CommonOptions
commonOptions =
    CommonOptions
        <$> strOption
            ( long "apikey"
                <> short 'k'
                <> metavar "APIKEY"
                <> help "API key for USDA FoodData Central"
            )

subcommand :: Parser SubCommand
subcommand =
    subparser
        ( command
            "fetch"
            ( info
                (Fetch <$> fetch)
                (progDesc "fetch a food by ID")
            )
            <> command
                "dump"
                ( info
                    (Dump <$> dump)
                    (progDesc "dump JSON for food by ID")
                )
            <> command
                "export"
                ( info
                    (Export <$> export)
                    (progDesc "export data for aggregated meal(s) in tabular form")
                )
            <> command
                "summarize"
                ( info
                    (Summarize <$> summarize)
                    (progDesc "summarize nutrients for a given time period")
                )
        )

fetch :: Parser FetchOptions
fetch = undefined

dump :: Parser DumpOptions
dump = undefined

export :: Parser ExportOptions
export = undefined

summarize :: Parser SummarizeOptions
summarize = undefined

parse :: Options -> IO ()
parse (Options c s) = case s of
    Fetch o -> runFetch c o
    Dump o -> runDump c o
    Export o -> runExport c o
    Summarize o -> runSummarize c o

data Options = Options CommonOptions SubCommand

data CommonOptions = CommonOptions
    {coKey :: T.Text}

data SubCommand
    = Fetch FetchOptions
    | Dump DumpOptions
    | Export ExportOptions
    | Summarize SummarizeOptions

data FetchOptions = FetchOptions
data DumpOptions = DumpOptions
data ExportOptions = ExportOptions
data SummarizeOptions = SummarizeOptions

runFetch = undefined
runDump = undefined
runExport = undefined
runSummarize = undefined
