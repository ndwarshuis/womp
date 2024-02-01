module Main (main) where

import qualified Data.Aeson as A
import Data.Char (ord)
import qualified Data.Csv as C
import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
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
      . setLogMinLevel (if coVerbosity then LevelDebug else LevelInfo)
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

runFetch :: FetchDumpOptions -> RIO SimpleApp ()
runFetch FetchDumpOptions {foID, foForce, foKey} =
  void . (\k -> fetchFID foForce k foID) =<< getStoreAPIKey foKey

runDump :: FetchDumpOptions -> RIO SimpleApp ()
runDump FetchDumpOptions {foID, foForce, foKey} = do
  k <- getStoreAPIKey foKey
  j <- fetchFID foForce k foID
  liftIO $ TI.putStr j

runExportTabular :: TabularExportOptions -> RIO SimpleApp ()
runExportTabular tos = go =<< readTrees (teoExport tos)
  where
    go = liftIO . BL.putStr . treeToCSV tsvOptions (teoGroup tos)

runExportTree :: TreeExportOptions -> RIO SimpleApp ()
runExportTree t@TreeExportOptions {teoJSON, teoTabularExport} = do
  ts <- readTrees $ teoExport teoTabularExport
  liftIO $ go $ treeToJSON (allTreeDisplayOpts t) gos ts
  where
    gos = teoGroup teoTabularExport
    go = if teoJSON then BL.putStr . A.encode else B.putStr . Y.encode

allTreeDisplayOpts :: TreeExportOptions -> AllTreeDisplayOptions
allTreeDisplayOpts
  TreeExportOptions
    { teoDisplay
    , teoTabularExport = TabularExportOptions {teoShowUnknowns, teoUnityUnits}
    } =
    AllTreeDisplayOptions teoDisplay teoShowUnknowns teoUnityUnits

readTrees
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => ExportOptions
  -> m (NonEmpty (DisplayTree GroupByAll))
readTrees ExportOptions {eoForce, eoMealPath, eoDateInterval, eoThreads, eoKey} = do
  setNumCapabilities eoThreads
  ds <- dateIntervalToDaySpan eoDateInterval
  readDisplayTrees eoForce eoKey ds eoMealPath (dioNormalize eoDateInterval)

runListNutrients :: MonadUnliftIO m => m ()
runListNutrients = BL.putStr $ C.encodeDefaultOrderedByNameWith tsvOptions dumpNutrientTree

-- TODO not DRY
runSummarize
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => ExportOptions
  -> m ()
runSummarize ExportOptions {eoForce, eoMealPath, eoDateInterval, eoThreads, eoKey} = do
  setNumCapabilities eoThreads
  ds <- dateIntervalToDaySpan eoDateInterval
  s <- readSummary eoForce eoKey ds eoMealPath (dioNormalize eoDateInterval)
  BL.putStr $ C.encodeDefaultOrderedByNameWith tsvOptions $ N.toList s

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

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }
