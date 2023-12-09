module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import qualified Data.Text.IO as TI
import qualified Dhall as D
import Internal.Types.Dhall
import Internal.Types.FDC
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import Options.Applicative
import RIO
-- import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import RIO.Char
import RIO.FilePath
import qualified RIO.Text as T
import UnliftIO.Directory

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
    <$> optional
      ( strOption
          ( long "apikey"
              <> short 'k'
              <> metavar "APIKEY"
              <> help "API key for USDA FoodData Central"
          )
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

-- TODO need to get this to an int somehow
fetch :: Parser FetchOptions
fetch =
  FetchOptions
    <$> option
      auto
      ( long "fid"
          <> short 'i'
          <> metavar "FOODID"
          <> help "ID for the food to pull from the database"
      )
    <*> flag False True (long "force" <> short 'f' <> help "force retrieve")

dump :: Parser DumpOptions
dump =
  DumpOptions
    <$> option
      auto
      ( long "fid"
          <> short 'i'
          <> metavar "FOODID"
          <> help "ID for the food to pull from the database"
      )
    <*> switch (long "force" <> short 'f' <> help "force retrieve")

export :: Parser ExportOptions
export =
  ExportOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> help "path to config with schedules and meals"
      )

summarize :: Parser SummarizeOptions
summarize = undefined

parse :: MonadUnliftIO m => Options -> m ()
parse (Options c s) = do
  logOpts <-
    setLogVerboseFormat True . setLogUseTime True
      <$> logOptionsHandle stderr False
  withLogFunc logOpts $ \lf -> do
    env <- mkSimpleApp lf Nothing
    runRIO env $ case s of
      Fetch o -> runFetch c o
      Dump o -> runDump c o
      Export o -> runExport c o

-- Summarize o -> runSummarize c o

data Options = Options CommonOptions SubCommand

data CommonOptions = CommonOptions
  { coKey :: (Maybe APIKey)
  }

type FID = Int

-- newtype FID = FID Int deriving (Read, Show)

newtype APIKey = APIKey {unAPIKey :: T.Text} deriving (IsString)

data SubCommand
  = Fetch !FetchOptions
  | Dump !DumpOptions
  | Export !ExportOptions
  | Summarize !SummarizeOptions

data FetchOptions = FetchOptions {foID :: !FID, foForce :: !Bool}

data DumpOptions = DumpOptions {doID :: !FID, doForce :: !Bool}

data ExportOptions = ExportOptions {eoConfig :: !FilePath}

data SummarizeOptions

runFetch :: CommonOptions -> FetchOptions -> RIO SimpleApp ()
runFetch CommonOptions {coKey} FetchOptions {foID, foForce} = do
  res <- getStoreAPIKey coKey
  case res of
    -- TODO throw a real error here
    Nothing -> undefined
    Just k -> void $ runFetch_ foForce foID k

runFetch_ :: MonadUnliftIO m => Bool -> FID -> APIKey -> m T.Text
runFetch_ frc i k = do
  f <- cacheDir
  let p = f </> (show i ++ ".json")
  if frc
    then getFoodJSON k i
    else do
      j <- readAndCache p (Just <$> getFoodJSON k i)
      case j of
        Nothing -> undefined
        Just j' -> return j'

runDump :: CommonOptions -> DumpOptions -> RIO SimpleApp ()
runDump CommonOptions {coKey} DumpOptions {doID, doForce} = do
  -- TODO not DRY
  res <- getStoreAPIKey coKey
  case res of
    -- TODO throw a real error here
    Nothing -> undefined
    Just k -> do
      j <- runFetch_ doForce doID k
      liftIO $ TI.putStr j

runExport :: CommonOptions -> ExportOptions -> RIO SimpleApp ()
runExport co ExportOptions {eoConfig} = do
  sch <- readConfig eoConfig
  rs <- concat <$> mapM (scheduleToTable co) sch
  BL.putStr $ C.encodeWith tsvOptions rs
  return ()

scheduleToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> Schedule
  -> m [RowNutrient]
scheduleToTable co Schedule {schMeal = Meal {mlIngs}} =
  concat <$> mapM (ingredientToTable co) mlIngs

-- TODO what do I want this table to look like?
ingredientToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> Ingredient
  -> m [RowNutrient]
ingredientToTable CommonOptions {coKey} Ingredient {ingFID} = do
  res <- getStoreAPIKey coKey
  case res of
    -- TODO throw a real error here
    Nothing -> logError "well crap" >> return []
    Just k -> do
      j <- runFetch_ False (fromIntegral ingFID) k
      let p = A.eitherDecodeStrict $ encodeUtf8 j
      -- B.putStr $ encodeUtf8 $ tshow (p :: Either String FoodItem)
      case p of
        Right r -> return $ toRowNutrients r
        Left e -> do
          logError $ displayBytesUtf8 $ BC.pack e
          return []

toRowNutrients :: FoodItem -> [RowNutrient]
toRowNutrients (Foundation FoundationFoodItem {ffiFdcId, ffiDescription, ffiFoodNutrients}) =
  go <$> fromMaybe [] ffiFoodNutrients
  where
    go FoodNutrient {fnNutrient, fnAmount} =
      RowNutrient
        { rnDesc = ffiDescription
        , rnId = ffiFdcId
        , rnNutrientId = nId =<< fnNutrient
        , rnNutrientName = nName =<< fnNutrient
        , rnUnit = nUnitName =<< fnNutrient
        , rnAmount = fnAmount
        }
toRowNutrients (Branded BrandedFoodItem {bfiFdcId, bfiDescription, bfiFoodNutrients}) =
  go <$> fromMaybe [] ffiFoodNutrients
toRowNutrients (SRLegacy SRLegacyFoodItem {}) = []
toRowNutrients (Survey SurveyFoodItem {}) = []

data RowNutrient = RowNutrient
  { rnId :: Int
  , rnDesc :: T.Text
  , rnNutrientName :: Maybe T.Text
  , rnNutrientId :: Maybe Int
  , rnAmount :: Maybe Double
  , rnUnit :: Maybe T.Text
  }
  deriving (Generic, Show)

instance C.ToRecord RowNutrient

-- runSummarize :: CommonOptions -> SummarizeOptions -> RIO SimpleApp ()
-- runSummarize = undefined

configDir :: MonadUnliftIO m => m FilePath
configDir = getXdgDirectory XdgConfig "carbon"

cacheDir :: MonadUnliftIO m => m FilePath
cacheDir = getXdgDirectory XdgCache "carb0n"

getFoodJSON :: MonadUnliftIO m => APIKey -> FID -> m T.Text
getFoodJSON k i = R.runReq R.defaultHttpConfig $ do
  res <- R.req R.GET url R.NoReqBody R.bsResponse opts
  return $ decodeUtf8Lenient $ R.responseBody res
  where
    url = apiFoodURL /~ tshow i
    opts = R.header "X-Api-Key" $ encodeUtf8 $ unAPIKey k

apiFoodURL :: R.Url 'R.Https
apiFoodURL =
  R.https "api.nal.usda.gov"
    /: "fdc"
    /~ ("v1" :: T.Text)
    /~ ("food" :: T.Text)

getStoreAPIKey :: MonadUnliftIO m => Maybe APIKey -> m (Maybe APIKey)
getStoreAPIKey k = do
  f <- (</> apiKeyFile) <$> configDir
  res <- readAndCache f (return $ unAPIKey <$> k)
  return $ APIKey <$> res

apiKeyFile :: FilePath
apiKeyFile = "apikey"

readAndCache :: MonadUnliftIO m => FilePath -> m (Maybe T.Text) -> m (Maybe T.Text)
readAndCache p x = do
  e <- doesFileExist p
  if e then Just <$> readFileUtf8 p else go =<< x
  where
    go Nothing = return Nothing
    go (Just t) = do
      createDirectoryIfMissing True $ takeDirectory p
      writeFileUtf8 p t
      return $ Just t

readConfig
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> m [Schedule]
readConfig f = do
  logInfo "boom"
  r <- liftIO $ D.inputFile D.auto f
  logInfo "poop"
  return r

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }
