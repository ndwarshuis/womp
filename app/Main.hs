module Main (main) where

import Control.Monad.Trans.Except
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import Data.Scientific
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
import qualified RIO.List as L
import qualified RIO.NonEmpty as N
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
      Summarize o -> runSummarize c o

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
  rs <- readRowNutrients co eoConfig
  BL.putStr $ C.encodeWith tsvOptions rs
  return ()

readRowNutrients :: CommonOptions -> FilePath -> RIO SimpleApp [RowNutrient]
readRowNutrients co p = do
  sch <- readConfig p
  concat <$> mapM (scheduleToTable co) sch

scheduleToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> Schedule
  -> m [RowNutrient]
scheduleToTable co Schedule {schMeal = Meal {mlIngs}} =
  concat <$> mapM (ingredientToTable co) mlIngs

-- TODO warn user if they attempt to get an experimentalal food (which is basically just an abstract)
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
toRowNutrients i = case i of
  (Foundation FoundationFoodItem {ffiMeta, ffiCommon}) ->
    go ffiMeta (flcCommon ffiCommon)
  (Branded BrandedFoodItem {bfiMeta, bfiCommon}) ->
    go bfiMeta bfiCommon
  (SRLegacy SRLegacyFoodItem {srlMeta, srlCommon}) ->
    go srlMeta (flcCommon srlCommon)
  where
    go m c = go' m <$> fcFoodNutrients c
    go' m FoodNutrient {fnNutrient, fnAmount, fnFoodNutrientDerivation} =
      RowNutrient
        { rnDesc = frmDescription m
        , rnId = frmId m
        , rnDerivation = ndDescription =<< fnFoodNutrientDerivation
        , rnNutrientId = nId =<< fnNutrient
        , rnNutrientName = nName =<< fnNutrient
        , rnUnit = nUnitName =<< fnNutrient
        , rnAmount = fnAmount
        }

data RowNutrient = RowNutrient
  { rnId :: Int
  , rnDesc :: T.Text
  , rnNutrientName :: Maybe T.Text
  , rnNutrientId :: Maybe Int
  , rnDerivation :: Maybe T.Text
  , rnAmount :: Maybe Scientific
  , rnUnit :: Maybe T.Text
  }
  deriving (Generic, Show)

data RowSum = RowSum
  { rsNutrientName :: T.Text
  , rsNutrientId :: Int
  , rsAmount :: Scientific
  , rsUnit :: T.Text
  }
  deriving (Generic, Show)

instance C.ToRecord RowNutrient

instance C.ToRecord RowSum

data Dimensional = Dimensional
  { dimValue :: Scientific
  , dimUnit :: Unit
  }
  deriving (Show, Eq)

data Unit = Unit
  { unitName :: UnitName
  , unitBase :: Prefix
  }
  deriving (Show, Eq)

sumRowNutrients :: [RowNutrient] -> Except T.Text [RowSum]
sumRowNutrients rs =
  mapM go $
    N.groupAllWith rsNutrientId $
      [ RowSum
        { rsAmount = v
        , rsUnit = u
        , rsNutrientName = n
        , rsNutrientId = i
        }
      | x@RowNutrient
          { rnAmount = Just v
          , rnUnit = Just u
          , rnNutrientId = Just i
          , rnNutrientName = Just n
          } <-
          rs
      ]
  where
    go ys@(y :| _) = do
      ds <- mapM (\x -> parseDimensional (rsAmount x) (rsUnit x)) ys
      d <- sumDimensionals ds
      let (newValue, newUnit) = fromDimensional d
      return $ y {rsAmount = newValue, rsUnit = newUnit}

sumDimensionals :: NonEmpty Dimensional -> Except T.Text Dimensional
sumDimensionals alld@(d :| ds) = do
  d' <- foldM addDimensional d ds
  return $
    d'
      { dimUnit =
          Unit
            { unitBase = majorityBase
            , unitName = unitName $ dimUnit d
            }
      }
  where
    majorityBase = majority $ fmap (unitBase . dimUnit) alld

majority :: Eq a => NonEmpty a -> a
majority = fst . N.head . N.reverse . N.sortWith snd . count

count :: Eq a => NonEmpty a -> NonEmpty (a, Int)
count (x :| xs) =
  let (xs', ys) = L.partition (x ==) xs
      thisCount = (x, length xs')
   in case N.nonEmpty ys of
        Nothing -> thisCount :| []
        Just ys' -> thisCount :| (N.toList (count ys'))

addDimensional :: Dimensional -> Dimensional -> Except T.Text Dimensional
addDimensional x@(Dimensional v0 u0) y@(Dimensional v1 _)
  | uname x == uname y = return $ Dimensional (v0 + v1) u0
  | otherwise =
      throwE $
        T.append
          (T.unwords ["could not add", tshow x, "and", tshow y])
          ": units don't match"
  where
    uname = unitName . dimUnit

fromDimensional :: Dimensional -> (Scientific, T.Text)
fromDimensional Dimensional {dimValue, dimUnit = u@Unit {unitBase}} =
  (raisePower dimValue (-(prefixValue unitBase)), tunit u)

tdimensional :: Dimensional -> T.Text
tdimensional d = let (v, u) = fromDimensional d in T.unwords [tshow v, u]

raisePower :: Scientific -> Int -> Scientific
raisePower s x = scientific (coefficient s) (base10Exponent s + x)

tunit :: Unit -> Text
tunit Unit {unitName, unitBase} = T.append n b
  where
    n = case unitName of
      Calorie -> "cal"
      Joule -> "J"
      Gram -> "g"
    b = case unitBase of
      Nano -> "n"
      Micro -> "µ"
      Milli -> "m"
      Centi -> "c"
      Deci -> "d"
      Unity -> ""
      Deca -> "da"
      Hecto -> "h"
      Kilo -> "k"
      Mega -> "M"
      Giga -> "G"

parseDimensional :: Scientific -> Text -> Except T.Text Dimensional
parseDimensional n u = do
  u' <- parseUnit u
  let n' = raisePower n (prefixValue $ unitBase u')
  return $ Dimensional n' u'

parseUnit :: T.Text -> Except T.Text Unit
parseUnit s = case T.splitAt 1 s of
  ("G", rest) -> parseName Giga rest <|> def
  ("M", rest) -> parseName Mega rest <|> def
  ("k", rest) -> parseName Kilo rest <|> def
  ("h", rest) -> parseName Hecto rest <|> def
  ("d", rest) -> parseName Deci rest <|> def
  ("c", rest) -> parseName Centi rest <|> def
  ("m", rest) -> parseName Milli rest <|> def
  ("µ", rest) -> parseName Micro rest <|> def
  ("n", rest) -> parseName Nano rest <|> def
  _ -> case T.splitAt 2 s of
    ("da", rest) -> parseName Deca rest <|> def
    _ -> def
  where
    def = parseName Unity s
    parseName p r = case r of
      "cal" -> return $ Unit Calorie p
      "g" -> return $ Unit Gram p
      "J" -> return $ Unit Joule p
      _ -> throwE $ T.append "could not parse unit: " s

data UnitName = Gram | Calorie | Joule
  deriving (Show, Eq)

prefixValue :: Prefix -> Int
prefixValue Nano = -9
prefixValue Micro = -6
prefixValue Milli = -3
prefixValue Centi = -2
prefixValue Deci = -1
prefixValue Unity = 0
prefixValue Deca = 1
prefixValue Hecto = 2
prefixValue Kilo = 3
prefixValue Mega = 6
prefixValue Giga = 9

data Prefix
  = Nano
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
  deriving (Show, Eq)

runSummarize :: CommonOptions -> SummarizeOptions -> RIO SimpleApp ()
runSummarize = sumRowNutrients

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
