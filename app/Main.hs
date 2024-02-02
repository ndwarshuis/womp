module Main (main) where

import qualified Data.Aeson as A
import Data.Char (ord)
import qualified Data.Csv as C
import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
import GHC.Conc (getNumProcessors)
import Internal.CLI
import Internal.Nutrient
import Internal.NutrientTree
import Internal.Types.Main
import Internal.Utils
import Options.Applicative
import RIO hiding (force)
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.NonEmpty as N
import RIO.Time
import UnliftIO.Concurrent

main :: IO ()
main = run =<< parseCLI

run :: MonadUnliftIO m => CLIOptions -> m ()
run (CLIOptions CommonOptions {coVerbosity} s) = do
  logOpts <-
    setLogVerboseFormat True
      . setLogUseTime False
      . setLogMinLevel (level coVerbosity)
      <$> logOptionsHandle stderr False
  withLogFunc logOpts $ \lf -> do
    env <- mkSimpleApp lf Nothing
    runRIO env $ handle err $ case s of
      Fetch o -> runFetch o
      Dump o -> runDump o
      ExportTabular o -> runExportTabular o
      ExportTree o -> runExportTree o
      ListNutrients -> runListNutrients
      Summarize o -> runSummarize o
  where
    err (AppException es) = do
      mapM_ (logError . displayBytesUtf8 . encodeUtf8) $ concatMap showError es
      exitFailure
    level x
      | x == 0 = LevelError
      | x == 1 = LevelWarn
      | x == 2 = LevelInfo
      | otherwise = LevelDebug

runFetch :: FetchDumpOptions -> RIO SimpleApp ()
runFetch FetchDumpOptions {foID, foForce, foKey} =
  void . (\k -> fetchFID foForce k foID) =<< getStoreAPIKey foKey

runDump :: FetchDumpOptions -> RIO SimpleApp ()
runDump FetchDumpOptions {foID, foForce, foKey} = do
  k <- getStoreAPIKey foKey
  j <- fetchFID foForce k foID
  liftIO $ TI.putStr j

runExportTabular :: TabularExportOptions -> RIO SimpleApp ()
runExportTabular tos@TabularExportOptions {tabCommonExport, tabSort, tabHeader} = do
  case parseSortKeys tabSort of
    Nothing -> throwAppErrorIO (SortKeys tabSort)
    Just ks -> go ks =<< readTrees (ceoExport tabCommonExport)
  where
    go ks =
      liftIO
        . BL.putStr
        . treeToCSV
          (allTabularDisplayOpts ks tos)
          (tsvOptions tabHeader)
          (ceoGroup tabCommonExport)

runExportTree :: TreeExportOptions -> RIO SimpleApp ()
runExportTree t@TreeExportOptions {treeJSON, treeCommonExport} = do
  ts <- readTrees $ ceoExport treeCommonExport
  liftIO $ go $ treeToJSON (allTreeDisplayOpts t) gos ts
  where
    gos = ceoGroup treeCommonExport
    go = if treeJSON then BL.putStr . A.encode else B.putStr . Y.encode

allTreeDisplayOpts :: TreeExportOptions -> AllTreeDisplayOptions
allTreeDisplayOpts
  TreeExportOptions
    { treeDisplay
    , treeCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoUnityUnits
        , ceoExport = ExportOptions {eoRoundDigits}
        }
    } =
    AllTreeDisplayOptions treeDisplay ceoShowUnknowns ceoUnityUnits eoRoundDigits

allTabularDisplayOpts :: [SortKey] -> TabularExportOptions -> AllTabularDisplayOptions
allTabularDisplayOpts
  ks
  TabularExportOptions
    { tabCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoUnityUnits
        , ceoExport = ExportOptions {eoRoundDigits}
        }
    } =
    AllTabularDisplayOptions ceoShowUnknowns ceoUnityUnits eoRoundDigits ks

readTrees
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => ExportOptions
  -> m (NonEmpty (DisplayTree GroupByAll))
readTrees ExportOptions {eoForce, eoMealPath, eoDateInterval, eoThreads, eoKey} = do
  setThreads eoThreads
  ds <- dateIntervalToDaySpan eoDateInterval
  readDisplayTrees eoForce eoKey ds eoMealPath (dioNormalize eoDateInterval)

setThreads :: MonadUnliftIO m => Int -> m ()
setThreads n
  | n > 0 = setNumCapabilities n
  | otherwise = setNumCapabilities =<< liftIO getNumProcessors

runListNutrients :: MonadUnliftIO m => m ()
runListNutrients =
  BL.putStr $ C.encodeDefaultOrderedByNameWith (tsvOptions True) dumpNutrientTree

-- TODO not DRY
runSummarize
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => SummarizeOptions
  -> m ()
runSummarize
  SummarizeOptions
    { soHeader
    , soExportOptions =
      ExportOptions
        { eoForce
        , eoMealPath
        , eoDateInterval
        , eoThreads
        , eoKey
        , eoRoundDigits
        }
    } = do
    setNumCapabilities eoThreads
    ds <- dateIntervalToDaySpan eoDateInterval
    s <- readSummary eoForce eoKey ds eoMealPath (dioNormalize eoDateInterval) eoRoundDigits
    BL.putStr $ C.encodeDefaultOrderedByNameWith (tsvOptions soHeader) $ N.toList s

dateIntervalToDaySpan :: MonadUnliftIO m => DateIntervalOptions -> m (NonEmpty DaySpan)
dateIntervalToDaySpan DateIntervalOptions {dioStart, dioEnd, dioDays, dioInterval} = do
  start <- maybe currentDay return dioStart
  totalLen <- combineErrorIO2 (getLen start) checkInterval const
  let span1 = genSpans 1 totalLen start
  return $ case dioInterval of
    Just i
      | i >= totalLen -> span1
      | otherwise ->
          let (n, r) = divMod totalLen i
              lastStart = addDays (fromIntegral $ n * i) start
              last = if r == 0 then [] else N.toList $ genSpans 1 r lastStart
           in append (genSpans n i start) last
    Nothing -> span1
  where
    getLenStartEnd start end = do
      when (end <= start) $ throwAppErrorIO DaySpanError
      return $ fromIntegral $ diffDays end start
    getLenDays = do
      when (dioDays < 1) $ throwAppErrorIO (DateDaysEndError dioDays)
      return dioDays
    getLen start = maybe getLenDays (getLenStartEnd start) dioEnd
    checkInterval = case dioInterval of
      Nothing -> return ()
      Just i -> when (i < 1) $ throwAppErrorIO $ IntervalError i
    genSpans n s = take1 n . fmap (,s - 1) . N.iterate (addDays $ fromIntegral s)

take1 :: Int -> NonEmpty a -> NonEmpty a
take1 n (x :| xs) = x :| take (n - 1) xs

currentDay :: MonadUnliftIO m => m Day
currentDay = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ localDay $ utcToLocalTime z u

tsvOptions :: Bool -> C.EncodeOptions
tsvOptions h =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = h
    }
