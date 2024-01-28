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
import Internal.Types.BiNonEmpty (BiNonEmpty)
import qualified Internal.Types.BiNonEmpty as BN
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
  -> m (NonEmpty (SpanFood NutrientMass NutrientEnergy))
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

groupByTup_ :: Eq a => [(a, b)] -> [(a, NonEmpty b)]
groupByTup_ =
  fmap (\xs -> (fst $ N.head xs, snd <$> xs))
    . N.groupWith fst

flattenNonEmpty :: NonEmpty (a, NonEmpty b) -> NonEmpty (a, b)
flattenNonEmpty = sconcat . fmap (\(x, ys) -> (x,) <$> ys)

flattenNonEmpty_ :: [(a, NonEmpty b)] -> [(a, b)]
flattenNonEmpty_ = concatMap (\(x, ys) -> (x,) <$> N.toList ys)

something
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Bool
  -> APIKey
  -> NonEmpty (ValidSchedule (Cron, Double))
  -> NonEmpty DaySpan
  -> m (NonEmpty (SpanFood NutrientMass NutrientEnergy))
something frc k vs ds = do
  -- current steps
  -- 1. get cartesian product of schedules and date intervals; this is necessary
  --    to do first since some ingredients may not be needed depending on which
  --    dates are selected
  -- 2. flip ingredients out of schedules and group together (so next step can
  --    run sanely in parallel); keep list of dates and scales under each
  --    ingredient since each ingredient may have multiople dates and scales
  --    associated with it
  -- 3. get json blobs for all ingredients (requires network)
  -- 4. convert json to food
  -- 5. expand food by dates and scales
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
  -> [(ValidSchedule Scientific, DaySpan)]
expandSchedule vs = concatMap go . toList
  where
    go ds = fmap (,ds) $ mapMaybe (go' ds) $ toList vs
    go' ds v@ValidSchedule {vsMeta = (w, s)} =
      fmap ((\s' -> v {vsMeta = s'}) . (* fromFloatDigits s) . fromIntegral . length) $
        N.nonEmpty $
          expandCronPat ds w

expandScheduleIO
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => NonEmpty (ValidSchedule (Cron, Double))
  -> NonEmpty DaySpan
  -> m (NonEmpty (ValidSchedule Scientific, DaySpan))
expandScheduleIO vs ds =
  maybe go return $ N.nonEmpty $ expandSchedule vs ds
  where
    go = do
      logError "Schedule does not intersect with desired date range"
      exitFailure

type IngMeta = (Mass, [Modification])

type GroupedIngredient a =
  ( a
  , NonEmpty (IngMeta, NonEmpty (Scientific, NonEmpty DaySpan))
  )

-- TODO clean this up
expandIngredients
  :: NonEmpty (ValidSchedule Scientific, DaySpan)
  -> BiNonEmpty (GroupedIngredient FID) (GroupedIngredient CustomSource)
expandIngredients =
  bimap
    (second (fmap (second groupByTup) . groupByTup))
    (second (fmap (second groupByTup) . groupByTup))
    . BN.swap
    . BN.groupByTup
    . BN.swap
    . BN.groupByTup
    . sconcat
    . fmap (uncurry go)
  where
    go ValidSchedule {vsIngs, vsMeta} ds = BN.fromNonEmpty (splitIng vsMeta ds) vsIngs
    splitIng scale ds Ingredient {ingMass, ingModifications, ingSource} =
      let meta = (Mass $ fromFloatDigits ingMass, ingModifications)
          rest = (meta, (scale, ds))
       in case ingSource of
            FDC i -> Left (FID i, rest)
            Custom c -> Right (c, rest)

-- let s' = case s of
--       FDC i -> Right $ FID $ fromIntegral i
--       Custom c -> undefined
--  in (s', (fromFloatDigits m, ms))

expandFood
  :: NonEmpty (FinalGroupedFood, NonEmpty (DaySpan, Scientific))
  -> NonEmpty (SpanFood NutrientMass NutrientEnergy)
expandFood =
  fmap (\(d, fs) -> SpanFood (sconcat fs) d)
    . groupByTup
    . fmap (\(f, (d, s)) -> (d, fmap (scaleNV s) f))
    . flattenNonEmpty

runMealState
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => FID
  -> [Modification]
  -> Mass
  -> ParsedFoodItem
  -> m FinalGroupedFood
runMealState i ms mass pfi = do
  let (ws, mfi) = mapFoodItem $ filterFoodItem pfi
  let (final, unused) = ingredientToTree ms mass mfi
  mapM_ (logWarn . displayText . fmtWarning i) ws
  logNutrientMap (Just i) unused
  return final

logNutrientMap :: MonadUnliftIO m => Maybe FID -> NutrientMap -> m ()
logNutrientMap i =
  mapM_ (logDebug . displayText . uncurry (fmtUnused i)) . M.toList

fmtUnused :: Maybe FID -> NID -> ValidNutrient -> T.Text
fmtUnused fi ni n =
  T.concat
    ( [ "Unused nutrient with id "
      , tshow ni
      ]
        ++ maybe [] ((: []) . T.append " in food with id " . tshow) fi
        ++ [ ": "
           , tshow n
           ]
    )

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

-- partitionIngredients
--   :: NonEmpty Ingredient
--   -> (BiNonEmpty ValidFDCIngredient CustomIngredient)
-- partitionIngredients = partitionBiNonEmpty go
--   where
--     go Ingredient {ingSource = s, ingMass = m, ingModifications = ms} =
--       case s of
--         FDC i -> Left $ ValidFDCIngredient (FID i) (fromFloatDigits m) ms
--         Custom c -> Right $ CustomIngredient ms (fromFloatDigits m) c

-- Left e -> throwAppErrorIO $ CustomIngError e
-- Right (f, nm) -> do
--   logNutrientMap Nothing nm
--   return $ Right f

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }
