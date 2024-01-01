module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Csv as C
import Data.Scientific
import qualified Data.Text.IO as TI
import qualified Dhall as D
import Internal.Nutrient
import Internal.NutrientTree
import Internal.Types.Dhall
import Internal.Types.Main
import Internal.Utils
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import Options.Applicative
import RIO hiding (force)
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath
import qualified RIO.NonEmpty as N
import RIO.State
import qualified RIO.Text as T
import RIO.Time
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

-- TODO not dry
export :: Parser ExportOptions
export =
  ExportOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> help "path to config with schedules and meals"
      )
    <*> dateInterval

summarize :: Parser SummarizeOptions
summarize =
  SummarizeOptions
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIG"
          <> help "path to config with schedules and meals"
      )
    <*> dateInterval
    <*> force
    <*> displayOptions
    <*> switch
      ( long "json"
          <> short 'j'
          <> help "summarize output in JSON (display options are ignored)"
      )

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

displayOptions :: Parser DisplayOptions
displayOptions =
  DisplayOptions
    <$> switch
      ( long "unknowns"
          <> short 'u'
          <> help "display unknown nutrients in output"
      )
    <*> option
      auto
      ( long "indent"
          <> short 'i'
          <> metavar "INDENT"
          <> help "indent level for output"
          <> value 2
      )

startDay :: Parser (Maybe Day)
startDay =
  fmap readDay
    <$> optional
      ( strOption
          ( long "start"
              <> short 's'
              <> metavar "START"
              <> help "start date on which to begin summary calculations"
          )
      )

endDay :: Parser (Maybe Day)
endDay =
  fmap readDay
    <$> optional
      ( strOption
          ( long "end"
              <> short 'e'
              <> metavar "END"
              <> help "end date on which to stop summary calculations (exclusive)"
          )
      )

parse :: MonadUnliftIO m => Options -> m ()
parse (Options c@CommonOptions {coVerbosity} s) = do
  logOpts <-
    setLogVerboseFormat True
      . setLogUseTime False
      . setLogMinLevel (if coVerbosity then LevelDebug else LevelInfo)
      <$> logOptionsHandle stderr False
  withLogFunc logOpts $ \lf -> do
    env <- mkSimpleApp lf Nothing
    runRIO env $ handle err $ case s of
      Fetch o -> runFetch c o
      Dump o -> runDump c o
      Export o -> runExport c o
      Summarize o -> runSummarize c o
  where
    err (AppException es) = do
      mapM_ (logError . displayBytesUtf8 . encodeUtf8) $ concatMap showError es
      exitFailure

readDay :: String -> Day
readDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

runFetch :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runFetch CommonOptions {coKey} FetchDumpOptions {foID, foForce} =
  void . runFetch_ foForce foID =<< getStoreAPIKey coKey

runFetch_
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> FID
  -> APIKey
  -> m Text
runFetch_ frc i k = do
  f <- cacheDir
  let p = f </> (show i ++ ".json")
  if frc
    then getFoodJSON k i
    else readAndCache p (getFoodJSON k i)

runDump :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runDump CommonOptions {coKey} FetchDumpOptions {foID, foForce} = do
  k <- getStoreAPIKey coKey
  j <- runFetch_ foForce foID k
  liftIO $ TI.putStr j

runExport :: CommonOptions -> ExportOptions -> RIO SimpleApp ()
runExport co ExportOptions {eoMealPath, eoDateInterval} = do
  dayspan <- dateIntervalToDaySpan eoDateInterval
  ts <- readTrees co eoMealPath dayspan
  case ts of
    Nothing -> return ()
    Just t -> do
      liftIO $ BL.putStr $ C.encodeWith tsvOptions $ nodesToRows t

runSummarize :: CommonOptions -> SummarizeOptions -> RIO SimpleApp ()
runSummarize co SummarizeOptions {soMealPath, soDateInterval, soDisplay, soJSON} = do
  dayspan <- dateIntervalToDaySpan soDateInterval
  ts <- readTrees co soMealPath dayspan
  let out =
        if soJSON
          then BL.putStr . A.encode . finalToJSON
          else TI.putStr . fmtFullTree soDisplay
  maybe (return ()) (liftIO . out) ts

readTrees
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> FilePath
  -> DaySpan
  -> m (Maybe FinalFood)
readTrees co p ds = do
  ss <- readMealPlan p
  case ss of
    (x : xs) -> do
      init <- scheduleToTree co ds x
      ts <- foldM (\acc -> fmap (<> acc) . scheduleToTree co ds) init xs
      return $ Just ts
    [] -> return Nothing

-- TODO show debug info for start/end dates and such
scheduleToTree
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> DaySpan
  -> Schedule
  -> m FinalFood
scheduleToTree co ds Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} = do
  days <- fromEither $ expandCronPat ds schWhen
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  (r :| rs) <- mapErrorsIO (ingredientToTable co mlName) is
  let scale = fromFloatDigits $ fromIntegral (length days) * fromMaybe 1.0 schScale
  return $ fmap (fmap (* scale)) <$> foldr (<>) r rs

-- TODO warn user if they attempt to get an experimentalal food (which is basically just an abstract)
ingredientToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> Text
  -> Ingredient
  -> m FinalFood
ingredientToTable CommonOptions {coKey} _ Ingredient {ingFID, ingMass, ingModifications} = do
  k <- getStoreAPIKey coKey
  let fid = FID $ fromIntegral ingFID
  -- TODO don't hardcode force here
  j <- runFetch_ False fid k
  case A.eitherDecodeStrict $ encodeUtf8 j of
    Right r -> runMealState fid ingModifications ingMass r
    Left e -> throwAppErrorIO $ JSONError $ BC.pack e

runMealState
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => FID
  -> [Modification]
  -> Double
  -> FoodItem
  -> m FinalFood
runMealState i ms mass item = do
  let (t, ws) = runState (ingredientToTree ms mass item) []
  mapM_ (logWarn . displayBytesUtf8 . encodeUtf8 . fmtWarning i) ws
  return t

fmtWarning :: FID -> AppWarning -> T.Text
fmtWarning i (AppWarning t n) =
  T.unwords [msg, "in nutrient with id", tshow n, "in food with id", tshow i]
  where
    msg = case t of
      NotGram -> "Unit is not for mass"
      NoUnit -> "No unit provided"
      NoAmount -> "No amount provided"

dateIntervalToDaySpan :: MonadUnliftIO m => DateIntervalOptions -> m DaySpan
dateIntervalToDaySpan DateIntervalOptions {dioStart, dioEnd, dioDays} = do
  start <- maybe currentDay return dioStart
  let dayLen = maybe dioDays (\e -> fromIntegral $ diffDays e start) dioEnd
  when (dayLen < 1) $ throwAppErrorIO $ DaySpanError dayLen
  return (start, dayLen - 1)

currentDay :: MonadUnliftIO m => m Day
currentDay = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ localDay $ utcToLocalTime z u

configDir :: MonadUnliftIO m => m FilePath
configDir = getXdgDirectory XdgConfig "carbon"

cacheDir :: MonadUnliftIO m => m FilePath
cacheDir = getXdgDirectory XdgCache "carb0n"

-- TODO catch errors here
getFoodJSON
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => APIKey
  -> FID
  -> m Text
getFoodJSON k i = do
  logInfo $ displayBytesUtf8 $ encodeUtf8 $ T.append "downloading " (tshow i)
  res <-
    R.runReq R.defaultHttpConfig $
      R.req R.GET url R.NoReqBody R.bsResponse opts
  logInfo $ displayBytesUtf8 $ encodeUtf8 $ T.append "downloaded " (tshow i)
  return $ decodeUtf8Lenient $ R.responseBody res
  where
    url = apiFoodURL /~ tshow i
    opts = R.header "X-Api-Key" $ encodeUtf8 $ unAPIKey k

apiFoodURL :: R.Url 'R.Https
apiFoodURL =
  R.https "api.nal.usda.gov"
    /: "fdc"
    /~ ("v1" :: Text)
    /~ ("food" :: Text)

getStoreAPIKey :: MonadUnliftIO m => Maybe APIKey -> m APIKey
getStoreAPIKey k = do
  f <- (</> apiKeyFile) <$> configDir
  APIKey <$> case k of
    Just k' -> readAndCache f (return $ unAPIKey k')
    Nothing -> do
      e <- doesFileExist f
      if e then readFileUtf8 f else throwAppErrorIO $ MissingAPIKey f

apiKeyFile :: FilePath
apiKeyFile = "apikey"

readAndCache :: MonadUnliftIO m => FilePath -> m Text -> m Text
readAndCache p x = do
  e <- doesFileExist p
  if e then readFileUtf8 p else go =<< x
  where
    go t = do
      createDirectoryIfMissing True $ takeDirectory p
      writeFileUtf8 p t
      return t

readMealPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> m [Schedule]
readMealPlan f = do
  logDebug $
    displayBytesUtf8 $
      encodeUtf8 $
        T.append "reading schedule at path: " $
          T.pack f
  liftIO $ D.inputFile D.auto f

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }

type DaySpan = (Day, Int)

expandCronPat :: MonadAppError m => DaySpan -> Cron -> m [Day]
expandCronPat b Cron {cronYear, cronMonth, cronDay, cronWeekly} =
  combineError3 yRes mRes dRes $ \ys ms ds ->
    filter validWeekday $
      mapMaybe (uncurry3 toDay) $
        takeWhile (\((y, _), m, d) -> (y, m, d) <= (yb1, mb1, db1)) $
          dropWhile (\((y, _), m, d) -> (y, m, d) < (yb0, mb0, db0)) $
            [(y, m, d) | y <- (\y -> (y, isLeapYear y)) <$> ys, m <- ms, d <- ds]
  where
    yRes = case cronYear of
      Nothing -> return [yb0 .. yb1]
      Just pat -> do
        ys <- expandMDYPat (fromIntegral yb0) (fromIntegral yb1) pat
        return $ dropWhile (< yb0) $ fromIntegral <$> ys
    mRes = expandMD 12 cronMonth
    dRes = expandMD 31 cronDay
    (s, e) = fromDaySpan b
    (yb0, mb0, db0) = toGregorian s
    (yb1, mb1, db1) = toGregorian $ addDays (-1) e
    expandMD lim =
      fmap (fromIntegral <$>)
        . maybe (return [1 .. lim]) (expandMDYPat 1 lim)
    expandW (OnDay x) = [fromEnum x]
    expandW (OnDays xs) = fromEnum <$> xs
    ws = maybe [] expandW cronWeekly
    validWeekday = if null ws then const True else \day -> dayToWeekday day `elem` ws
    toDay (y, leap) m d
      | m == 2 && (not leap && d > 28 || leap && d > 29) = Nothing
      | m `elem` [4, 6, 9, 11] && d > 30 = Nothing
      | otherwise = Just $ fromGregorian y m d

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

expandMDYPat :: MonadAppError m => Natural -> Natural -> MDYPat -> m [Natural]
expandMDYPat lower upper (Single x) = return [x | lower <= x && x <= upper]
expandMDYPat lower upper (Multi xs) = return $ dropWhile (<= lower) $ takeWhile (<= upper) xs
expandMDYPat lower upper (After x) = return [max lower x .. upper]
expandMDYPat lower upper (Before x) = return [lower .. min upper x]
expandMDYPat lower upper (Between (Btw btStart btEnd)) =
  return [max lower btStart .. min upper btEnd]
expandMDYPat lower upper (Repeat RepeatPat {rpStart = s, rpBy = b, rpRepeats = r})
  | b < 1 = throwAppError $ DatePatternError s b r ZeroLength
  | otherwise = do
      k <- limit r
      return $ dropWhile (<= lower) $ takeWhile (<= k) [s + i * b | i <- [0 ..]]
  where
    limit Nothing = return upper
    limit (Just n)
      -- this guard not only produces the error for the user but also protects
      -- from an underflow below it
      | n < 1 = throwAppError $ DatePatternError s b r ZeroRepeats
      | otherwise = return $ min (s + b * (n - 1)) upper

fromDaySpan :: DaySpan -> (Day, Day)
fromDaySpan (d, n) = (d, addDays (fromIntegral n + 1) d)

dayToWeekday :: Day -> Int
dayToWeekday (ModifiedJulianDay d) = mod (fromIntegral d + 2) 7
