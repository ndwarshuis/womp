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
import RIO.FilePath
import qualified RIO.NonEmpty as N
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
      ExportTabular o -> runExportTabular c o
      ExportTree o -> runExportTree c o
      ListNutrients -> runListNutrients
      Summarize o -> runSummarize c o
  where
    err (AppException es) = do
      mapM_ (logError . displayBytesUtf8 . encodeUtf8) $ concatMap showError es
      exitFailure

runFetch :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runFetch CommonOptions {coKey} FetchDumpOptions {foID, foForce} =
  void . (\k -> fetchFID foForce k foID) =<< getStoreAPIKey coKey

runDump :: CommonOptions -> FetchDumpOptions -> RIO SimpleApp ()
runDump CommonOptions {coKey} FetchDumpOptions {foID, foForce} = do
  k <- getStoreAPIKey coKey
  j <- fetchFID foForce k foID
  liftIO $ TI.putStr j

runExportTabular :: CommonOptions -> TabularOptions -> RIO SimpleApp ()
runExportTabular co tos = go =<< readTrees co tos
  where
    go = liftIO . BL.putStr . treeToCSV tsvOptions (eoGroup tos)

runExportTree :: CommonOptions -> TreeOptions -> RIO SimpleApp ()
runExportTree co (TreeOptions dos j tos) = do
  liftIO . go . treeToJSON dos gos =<< readTrees co tos
  where
    gos = eoGroup tos
    go = if j then BL.putStr . A.encode else B.putStr . Y.encode

-- TODO need to normalize somehow
readTrees
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> TabularOptions
  -> m (NonEmpty (DisplayTree GroupByAll))
readTrees co TabularOptions {eoForce, eoMealPath, eoDateInterval, eoThreads} = do
  setNumCapabilities eoThreads
  k <- getStoreAPIKey $ coKey co
  ds <- dateIntervalToDaySpan eoDateInterval
  readDisplayTrees eoForce k ds eoMealPath (dioNormalize eoDateInterval)

-- n = dioNormalize eoDateInterval

runListNutrients :: MonadUnliftIO m => m ()
runListNutrients = BL.putStr $ C.encodeDefaultOrderedByNameWith tsvOptions dumpNutrientTree

-- TODO not DRY
runSummarize
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => CommonOptions
  -> TabularOptions
  -> m ()
runSummarize co TabularOptions {eoForce, eoMealPath, eoDateInterval, eoThreads} = do
  setNumCapabilities eoThreads
  k <- getStoreAPIKey $ coKey co
  ds <- dateIntervalToDaySpan eoDateInterval
  s <- readSummary eoForce k ds eoMealPath (dioNormalize eoDateInterval)
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

tsvOptions :: C.EncodeOptions
tsvOptions =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = True
    }
