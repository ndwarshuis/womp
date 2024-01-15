module Main (main) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import Data.Char (ord)
import qualified Data.Csv as C
import Data.Scientific
import Data.Semigroup (sconcat)
import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
import qualified Dhall as D
import Internal.CLI
import Internal.Nutrient
import Internal.NutrientTree
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import Options.Applicative
import RIO hiding (force)
import qualified RIO.ByteString.Lazy as BL
import RIO.FilePath
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import qualified RIO.Text as T
import RIO.Time
import UnliftIO.Concurrent
import UnliftIO.Directory

main :: IO ()
main = run =<< parseCLI

run :: MonadUnliftIO m => CLIOptions -> m ()
run (CLIOptions c@CommonOptions {coVerbosity} s) = do
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

runFetch :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runFetch CommonOptions {coKey} FetchDumpOptions {foID, foForce} =
  void . fetchFID foForce foID =<< getStoreAPIKey coKey

fetchFID
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> FID
  -> APIKey
  -> m Text
fetchFID frc i k = do
  e <- fidExists i
  if frc then go else if e then readFID i else go
  where
    go = downloadFID k i

fidPath :: MonadUnliftIO m => FID -> m FilePath
fidPath i = do
  d <- cacheDir
  return $ d </> (show i ++ ".json")

fidExists :: MonadUnliftIO m => FID -> m Bool
fidExists i = doesFileExist =<< fidPath i

downloadFID
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => APIKey
  -> FID
  -> m Text
downloadFID k i = do
  p <- fidPath i
  j <- getFoodJSON k i
  createWriteFile p j
  return j

-- | Read FID JSON file
-- ASSUME it has already been downloaded or this will error
readFID :: MonadUnliftIO m => FID -> m Text
readFID i = readFileUtf8 =<< fidPath i

runDump :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runDump CommonOptions {coKey} FetchDumpOptions {foID, foForce} = do
  k <- getStoreAPIKey coKey
  j <- fetchFID foForce foID k
  liftIO $ TI.putStr j

runExport :: CommonOptions -> ExportOptions -> RIO SimpleApp ()
runExport co xo = go =<< readTrees co xo
  where
    go =
      liftIO
        . BL.putStr
        . C.encodeDefaultOrderedByNameWith tsvOptions
        . sconcat
        . fmap nodesToRows

runSummarize :: CommonOptions -> SummarizeOptions -> RIO SimpleApp ()
runSummarize co (SummarizeOptions d j xo) = do
  liftIO . go =<< readTrees co xo
  where
    go =
      if j
        then BL.putStr . A.encode . fmap (finalToJSON d)
        else TI.putStr . sconcat . fmap (fmtFullTree d)

readTrees
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> ExportOptions
  -> m (NonEmpty SpanFood)
readTrees co ExportOptions {eoForce, eoMealPath, eoDateInterval, eoThreads} = do
  setNumCapabilities eoThreads
  k <- getStoreAPIKey $ coKey co
  ds <- dateIntervalToDaySpan eoDateInterval
  ss <- fmap go <$> readMealPlan eoMealPath
  something eoForce k ss ds
  where
    n = dioNormalize eoDateInterval
    go (ValidSchedule x (d, s)) = ValidSchedule x (d, s / fromIntegral n)

groupByTup :: Eq a => NonEmpty (a, b) -> NonEmpty (a, NonEmpty b)
groupByTup =
  fmap (\xs -> (fst $ N.head xs, snd <$> xs))
    . N.groupWith1 fst

flattenNonEmpty :: NonEmpty (a, NonEmpty b) -> NonEmpty (a, b)
flattenNonEmpty = sconcat . fmap (\(x, ys) -> (x,) <$> ys)

something
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Bool
  -> APIKey
  -> NonEmpty (ValidSchedule (Cron, Double))
  -> NonEmpty DaySpan
  -> m (NonEmpty SpanFood)
something frc k vs ds = do
  xs <- expandScheduleIO vs ds
  js <- mapPooledErrorsIO (\(x, y) -> (,y) <$> getJSON x) $ expandIngredients xs
  fs <- mapM (uncurry jsonToFood) $ flattenNonEmpty js
  return $ expandFood fs
  where
    getJSON fid = do
      j <- fetchFID frc fid k
      case A.eitherDecodeStrict $ encodeUtf8 j of
        Right r -> return (fid, r)
        Left e -> throwAppErrorIO $ JSONError $ BC.pack e
    jsonToFood (fid, r) ((m, ms), rest) = do
      f <- runMealState fid ms m r
      return (f, rest)

expandSchedule
  :: NonEmpty (ValidSchedule (Cron, Double))
  -> NonEmpty DaySpan
  -> [(DaySpan, NonEmpty (ValidSchedule Scientific))]
expandSchedule vs = mapMaybe go . toList
  where
    go ds = fmap (ds,) $ N.nonEmpty $ mapMaybe (go' ds) $ toList vs
    go' ds v@ValidSchedule {vsMeta = (w, s)} =
      fmap ((\s' -> v {vsMeta = s'}) . (* fromFloatDigits s) . fromIntegral . length) $
        N.nonEmpty $
          expandCronPat ds w

expandScheduleIO
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => NonEmpty (ValidSchedule (Cron, Double))
  -> NonEmpty DaySpan
  -> m (NonEmpty (DaySpan, NonEmpty (ValidSchedule Scientific)))
expandScheduleIO vs ds =
  maybe go return $ N.nonEmpty $ expandSchedule vs ds
  where
    go = do
      logError "Schedule does not intersect with desired date range"
      exitFailure

type IngMeta = (Scientific, [Modification])

expandIngredients
  :: NonEmpty (DaySpan, NonEmpty (ValidSchedule Scientific))
  -> NonEmpty (FID, NonEmpty (IngMeta, NonEmpty (DaySpan, Scientific)))
expandIngredients =
  fmap (second groupByTup)
    . groupByTup
    . fmap (\(d, (s, (f, fm))) -> (f, (fm, (d, s))))
    . flattenNonEmpty
    . fmap (second (flattenNonEmpty . fmap go))
  where
    go ValidSchedule {vsIngs, vsMeta} = (vsMeta, go' <$> vsIngs)
    go' Ingredient {ingMass = m, ingModifications = ms, ingFID = f} =
      (FID $ fromIntegral f, (fromFloatDigits m, ms))

expandFood
  :: NonEmpty (FinalFood, NonEmpty (DaySpan, Scientific))
  -> NonEmpty SpanFood
expandFood =
  fmap (\(d, fs) -> SpanFood (sconcat fs) d)
    . groupByTup
    . fmap (\(f, (d, s)) -> (d, fmap (scaleNV s) f))
    . flattenNonEmpty

runMealState
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => FID
  -> [Modification]
  -> Scientific
  -> ParsedFoodItem
  -> m FinalFood
runMealState i ms mass pfi = do
  let (ws, mfi) = mapFoodItem $ filterFoodItem pfi
  let (final, unused) = ingredientToTree ms mass mfi
  mapM_ (logWarn . displayText . fmtWarning i) ws
  mapM_ (logDebug . displayText . uncurry (fmtUnused i)) $ M.toList unused
  return final

displayText :: Text -> Utf8Builder
displayText = displayBytesUtf8 . encodeUtf8

fmtUnused :: FID -> NID -> ValidNutrient -> T.Text
fmtUnused fi ni n =
  T.concat
    [ "Unused nutrient with id "
    , tshow ni
    , " in food with id "
    , tshow fi
    , ": "
    , tshow n
    ]

fmtWarning :: FID -> NutrientWarning -> T.Text
fmtWarning fi (NotGram ni n) =
  T.unwords
    [ "Unit"
    , n
    , "is not a mass in nutrient with id"
    , tshow ni
    , "in food with id"
    , tshow fi
    ]
fmtWarning fi (UnknownUnit ni n) =
  T.unwords
    [ "Unit"
    , n
    , "cannot be parsed in nutrient with id"
    , tshow ni
    , "in food with id"
    , tshow fi
    ]
fmtWarning fi (InvalidNutrient n) =
  T.unwords ["Food with id", tshow fi, "has invalid food nutrient:", tshow n]

-- where
--   msg = case t of
--     NotGram -> "Unit is not for mass"
--     NoUnit -> "No unit provided"
--     NoAmount -> "No amount provided"

dateIntervalToDaySpan :: MonadUnliftIO m => DateIntervalOptions -> m (NonEmpty DaySpan)
dateIntervalToDaySpan DateIntervalOptions {dioStart, dioEnd, dioDays, dioInterval} = do
  start <- maybe currentDay return dioStart
  let totalLen = maybe dioDays (\e -> fromIntegral $ diffDays e start) dioEnd
  let span1 = genSpans 1 totalLen start
  when (totalLen < 1) $ throwAppErrorIO $ DaySpanError totalLen
  case dioInterval of
    Just i
      | i >= totalLen -> return span1
      | otherwise -> do
          when (i < 1) $ throwAppErrorIO $ IntervalError i
          let (n, r) = divMod totalLen i
          let spanN = genSpans n i start
          return $
            if r == 0
              then spanN
              else spanN <> genSpans 1 r (addDays (fromIntegral $ n * i) start)
    Nothing -> return span1
  where
    genSpans n s = take1 n . fmap (,s - 1) . N.iterate (addDays $ fromIntegral s)

take1 :: Int -> NonEmpty a -> NonEmpty a
take1 n (x :| xs) = x :| take (n - 1) xs

currentDay :: MonadUnliftIO m => m Day
currentDay = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ localDay $ utcToLocalTime z u

configDir :: MonadUnliftIO m => m FilePath
configDir = getXdgDirectory XdgConfig "womp"

cacheDir :: MonadUnliftIO m => m FilePath
cacheDir = getXdgDirectory XdgCache "womp"

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
  case k of
    Just k' -> do
      createWriteFile f (unAPIKey k')
      return k'
    Nothing -> do
      e <- doesFileExist f
      if e then APIKey <$> readFileUtf8 f else throwAppErrorIO $ MissingAPIKey f

apiKeyFile :: FilePath
apiKeyFile = "apikey"

createWriteFile :: MonadUnliftIO m => FilePath -> Text -> m ()
createWriteFile p t = do
  createDirectoryIfMissing True $ takeDirectory p
  writeFileUtf8 p t

readMealPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> m (NonEmpty (ValidSchedule (Cron, Double)))
readMealPlan f = do
  logDebug $
    displayBytesUtf8 $
      encodeUtf8 $
        T.append "reading schedule at path: " $
          T.pack f
  ss <-
    liftIO $
      if isDhall f
        then D.inputFile D.auto f
        else
          if isYaml f
            then Y.decodeFileThrow f
            else throwAppErrorIO $ FileTypeError f
  vs <- mapM checkSched ss
  maybe (logError "meal plan is empty" >> exitFailure) return $ N.nonEmpty vs

checkSched :: MonadUnliftIO m => Schedule -> m (ValidSchedule (Cron, Double))
checkSched Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} = do
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  fromEither $ checkCronPat schWhen
  return $ ValidSchedule is (schWhen, s)
  where
    s = fromMaybe 1.0 schScale

isDhall :: FilePath -> Bool
isDhall = isExtensionOf "dhall"

isYaml :: FilePath -> Bool
isYaml f = isExtensionOf "yaml" f || isExtensionOf "yml" f

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }

expandCronPat :: DaySpan -> Cron -> [Day]
expandCronPat b Cron {cronYear, cronMonth, cronDay, cronWeekly} =
  filter validWeekday $
    mapMaybe (uncurry3 toDay) $
      takeWhile (\((y, _), m, d) -> (y, m, d) <= (yb1, mb1, db1)) $
        dropWhile (\((y, _), m, d) -> (y, m, d) < (yb0, mb0, db0)) $
          [(y, m, d) | y <- (\y -> (y, isLeapYear y)) <$> ys, m <- ms, d <- ds]
  where
    ys = case cronYear of
      Nothing -> [yb0 .. yb1]
      Just pat ->
        dropWhile (< yb0) $
          fromIntegral
            <$> expandMDYPat (fromIntegral yb0) (fromIntegral yb1) pat
    ms = expandMD 12 cronMonth
    ds = expandMD 31 cronDay
    (s, e) = fromDaySpan b
    (yb0, mb0, db0) = toGregorian s
    (yb1, mb1, db1) = toGregorian $ addDays (-1) e
    expandMD lim =
      (fromIntegral <$>)
        . maybe [1 .. lim] (expandMDYPat 1 lim)
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

-- ASSUME that the repeat pattern is valid
expandMDYPat :: Natural -> Natural -> MDYPat -> [Natural]
expandMDYPat lower upper (Single x) = [x | lower <= x && x <= upper]
expandMDYPat lower upper (Multi xs) = dropWhile (<= lower) $ takeWhile (<= upper) xs
expandMDYPat lower upper (After x) = [max lower x .. upper]
expandMDYPat lower upper (Before x) = [lower .. min upper x]
expandMDYPat lower upper (Between (Btw btStart btEnd)) =
  [max lower btStart .. min upper btEnd]
expandMDYPat lower upper (Repeat RepeatPat {rpStart = s, rpBy = b, rpRepeats = r}) =
  -- ASSUME b and (Just r) > 0
  let k = maybe upper (\n -> min (s + b * (n - 1)) upper) r
   in dropWhile (<= lower) $ takeWhile (<= k) [s + i * b | i <- [0 ..]]

checkCronPat :: MonadAppError m => Cron -> m ()
checkCronPat Cron {cronYear, cronMonth, cronDay} =
  mapM_ (mapM go) [cronYear, cronMonth, cronDay]
  where
    go (Repeat p) = checkRepeatPat p
    go _ = return ()

checkRepeatPat :: MonadAppError m => RepeatPat -> m ()
checkRepeatPat RepeatPat {rpStart = s, rpBy = b, rpRepeats = r}
  | b == 0 = throwAppError $ DatePatternError s b r ZeroLength
  | r == Just 0 = throwAppError $ DatePatternError s b r ZeroRepeats
  | otherwise = return ()

fromDaySpan :: DaySpan -> (Day, Day)
fromDaySpan (d, n) = (d, addDays (fromIntegral n + 1) d)

dayToWeekday :: Day -> Int
dayToWeekday (ModifiedJulianDay d) = mod (fromIntegral d + 2) 7
