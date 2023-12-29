module Main (main) where

import Control.Monad.Error.Class
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.Csv as C
import Data.Scientific
import qualified Data.Text.IO as TI
import qualified Dhall as D
import Internal.Nutrient
import Internal.Types.Dhall
import Internal.Types.Main
import Internal.Utils
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

data Options = Options CommonOptions SubCommand

data CommonOptions = CommonOptions
  { coKey :: !(Maybe APIKey)
  , coVerbosity :: !Bool
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

data ExportOptions = ExportOptions
  { eoConfig :: !FilePath
  , eoDateInterval :: !DateIntervalOptions
  }

data SummarizeOptions = SummarizeOptions
  { soConfig :: !FilePath
  , soDateInterval :: !DateIntervalOptions
  }

data DateIntervalOptions = DateIntervalOptions
  { dioStart :: Maybe Day
  , dioEnd :: Maybe Day
  , dioDays :: Int
  }

readDay :: String -> Day
readDay = parseTimeOrError False defaultTimeLocale "%Y-%m-%d"

runFetch :: CommonOptions -> FetchOptions -> RIO SimpleApp ()
runFetch CommonOptions {coKey} FetchOptions {foID, foForce} = do
  res <- getStoreAPIKey coKey
  case res of
    -- TODO throw a real error here
    Nothing -> undefined
    Just k -> void $ runFetch_ foForce foID k

runFetch_
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> FID
  -> APIKey
  -> m T.Text
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
runExport co ExportOptions {eoConfig, eoDateInterval} = do
  dayspan <- dateIntervalToDaySpan eoDateInterval
  rs <- readRowNutrients co eoConfig dayspan
  BL.putStr $ C.encodeWith tsvOptions rs

readRowNutrients
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> FilePath
  -> DaySpan
  -> m [RowNutrient]
readRowNutrients co p ds = do
  sch <- readConfig p
  concat <$> mapErrorsIO (scheduleToTable co ds) sch

scheduleToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> DaySpan
  -> Schedule
  -> m [RowNutrient]
scheduleToTable co ds Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} = do
  days <- fromEither $ expandCronPat ds schWhen
  rs <- concat <$> mapM (ingredientToTable co mlName) mlIngs
  let scale = fromFloatDigits $ fromIntegral (length days) * fromMaybe 1.0 schScale
  return $ fmap (scaleRowNutrient scale) rs

-- TODO warn user if they attempt to get an experimentalal food (which is basically just an abstract)
ingredientToTable
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> T.Text
  -> Ingredient
  -> m [RowNutrient]
ingredientToTable CommonOptions {coKey} n Ingredient {ingFID, ingMass, ingModifications} = do
  res <- getStoreAPIKey coKey
  case res of
    -- TODO throw a real error here
    Nothing -> logError "well crap" >> return []
    Just k -> do
      j <- runFetch_ False (fromIntegral ingFID) k
      let p = A.eitherDecodeStrict $ encodeUtf8 j
      case p of
        Right r ->
          return $
            fmap applyScale $
              applyMods ingModifications $
                toRowNutrients n r
        Left e -> do
          logError $ displayBytesUtf8 $ BC.pack e
          return []
  where
    scale = fromFloatDigits ingMass / standardMass
    applyScale = scaleRowNutrient scale
    applyMods ms rs = fmap (\r -> foldr applyModification r ms) rs

scaleRowNutrient :: Scientific -> RowNutrient -> RowNutrient
scaleRowNutrient x r@RowNutrient {rnAmount} = r {rnAmount = (* x) <$> rnAmount}

-- TODO not sure if this ever changes
standardMass :: Scientific
standardMass = 100

applyModification :: Modification -> RowNutrient -> RowNutrient
applyModification
  Modification {modNutID, modScale}
  r@RowNutrient {rnNutrientId, rnAmount}
    | Just (fromIntegral modNutID) == rnNutrientId =
        r {rnAmount = (* fromFloatDigits modScale) <$> rnAmount}
    | otherwise = r

toRowNutrients :: T.Text -> FoodItem -> [RowNutrient]
toRowNutrients n i = case i of
  (Foundation FoundationFoodItem {ffiMeta, ffiCommon}) ->
    go ffiMeta (flcCommon ffiCommon)
  -- (Branded BrandedFoodItem {bfiMeta, bfiCommon}) ->
  --   go bfiMeta bfiCommon
  (SRLegacy SRLegacyFoodItem {srlMeta, srlCommon}) ->
    go srlMeta (flcCommon srlCommon)
  where
    go m c = go' m <$> fcFoodNutrients c
    go' m FoodNutrient {fnNutrient, fnAmount, fnFoodNutrientDerivation} =
      RowNutrient
        { rnDesc = frmDescription m
        , rnMealName = n
        , rnId = frmId m
        , rnDerivation = ndDescription =<< fnFoodNutrientDerivation
        , rnNutrientId = nId =<< fnNutrient
        , rnNutrientName = nName =<< fnNutrient
        , rnUnit = nUnitName =<< fnNutrient
        , rnAmount = fnAmount
        }

sumRowNutrients :: MonadAppError m => [RowNutrient] -> m [RowSum]
sumRowNutrients rs =
  mapM go $
    N.groupAllWith rsNutrientId $
      [ RowSum
        { rsAmount = v
        , rsUnit = u
        , rsNutrientName = n
        , rsNutrientId = i
        }
      | RowNutrient
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

sumDimensionals :: MonadAppError m => NonEmpty Dimensional -> m Dimensional
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
        Just ys' -> thisCount :| N.toList (count ys')

addDimensional :: MonadAppError m => Dimensional -> Dimensional -> m Dimensional
addDimensional x@(Dimensional v0 u0) y@(Dimensional v1 _)
  | uname x == uname y = return $ Dimensional (v0 + v1) u0
  | otherwise = throwAppError $ UnitMatchError x y
  where
    uname = unitName . dimUnit

fromDimensional :: Dimensional -> (Scientific, T.Text)
fromDimensional Dimensional {dimValue, dimUnit = u@Unit {unitBase}} =
  (raisePower dimValue (-(prefixValue unitBase)), tunit u)

-- tdimensional :: Dimensional -> T.Text
-- tdimensional d = let (v, u) = fromDimensional d in T.unwords [tshow v, u]

raisePower :: Scientific -> Int -> Scientific
raisePower s x = scientific (coefficient s) (base10Exponent s + x)

tunit :: Unit -> Text
tunit Unit {unitName, unitBase} = T.append prefix unit
  where
    unit = case unitName of
      Calorie -> "cal"
      Joule -> "J"
      Gram -> "g"
      IU -> "IU"
    prefix = case unitBase of
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

parseDimensional
  :: MonadAppError m
  => Scientific
  -> Text
  -> m Dimensional
parseDimensional n u = do
  u' <- parseUnit u
  let n' = raisePower n (prefixValue $ unitBase u')
  return $ Dimensional n' u'

runSummarize :: CommonOptions -> SummarizeOptions -> RIO SimpleApp ()
runSummarize co SummarizeOptions {soConfig, soDateInterval} = do
  dayspan <- dateIntervalToDaySpan soDateInterval
  rs <- readRowNutrients co soConfig dayspan
  ss <- fromEither $ sumRowNutrients rs
  BL.putStr $ C.encodeWith tsvOptions ss

dateIntervalToDaySpan :: MonadUnliftIO m => DateIntervalOptions -> m DaySpan
dateIntervalToDaySpan DateIntervalOptions {dioStart, dioEnd, dioDays} = do
  start <- maybe currentDay return dioStart
  let dayLen = maybe dioDays (\e -> fromIntegral $ diffDays e start) dioEnd
  when (dayLen < 1) $ throwAppErrorIO $ DaySpanError dayLen
  return (start, dayLen)

currentDay :: MonadUnliftIO m => m Day
currentDay = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ localDay $ utcToLocalTime z u

configDir :: MonadUnliftIO m => m FilePath
configDir = getXdgDirectory XdgConfig "carbon"

cacheDir :: MonadUnliftIO m => m FilePath
cacheDir = getXdgDirectory XdgCache "carb0n"

getFoodJSON
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => APIKey
  -> FID
  -> m T.Text
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
