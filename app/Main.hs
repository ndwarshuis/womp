module Main (main) where

import qualified Data.Aeson as A
import Data.Char (ord)
import qualified Data.Csv as C
import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
import Internal.CLI
import Internal.Display
import Internal.Export
import Internal.Ingest
import Internal.Types.CLI
import Internal.Types.Dhall
import Internal.Types.Main
import Internal.Utils
import Options.Applicative
import RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.NonEmpty as N
import qualified RIO.Text as T

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
      mapM_ (logError . displayText) $ concatMap showError es
      exitFailure
    level x
      | x == 0 = LevelError
      | x == 1 = LevelWarn
      | x == 2 = LevelInfo
      | otherwise = LevelDebug

runFetch
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => FetchDumpOptions
  -> m ()
runFetch FetchDumpOptions {foID, foForce, foKey} =
  void . (\k -> fetchFID foForce k foID) =<< getStoreAPIKey foKey

runDump
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => FetchDumpOptions
  -> m ()
runDump FetchDumpOptions {foID, foForce, foKey} = do
  k <- getStoreAPIKey foKey
  j <- fetchFID foForce k foID
  liftIO $ TI.putStr j

runExportTabular
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => TabularExportOptions
  -> m ()
runExportTabular tos@TabularExportOptions {tabCommonExport, tabSort, tabHeader} =
  combineErrorIOM2 readTrees getOpts $ \ts opts ->
    liftIO $
      BL.putStr $
        treeToCSV opts (tsvOptions tabHeader) (ceoGroup tabCommonExport) ts
  where
    readTrees = readDisplayTrees (ceoMealplan tabCommonExport)
    getOpts = do
      ks <- parseSortKeysIO tabSort
      allTabularDisplayOpts ks tos

runSummarize
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => SummarizeOptions
  -> m ()
runSummarize SummarizeOptions {soHeader, soMealplanOptions} = do
  s <- readSummary soMealplanOptions
  BL.putStr $ C.encodeDefaultOrderedByNameWith (tsvOptions soHeader) $ N.toList s

runExportTree
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => TreeExportOptions
  -> m ()
runExportTree t@TreeExportOptions {treeJSON, treeCommonExport} = do
  combineErrorIOM2 readTrees (allTreeDisplayOpts t) $ \ts atds ->
    liftIO $ go $ treeToJSON atds gos ts
  where
    readTrees = readDisplayTrees $ ceoMealplan treeCommonExport
    gos = ceoGroup treeCommonExport
    go = if treeJSON then BL.putStr . A.encode else B.putStr . Y.encode

runListNutrients :: MonadUnliftIO m => m ()
runListNutrients =
  BL.putStr $ C.encodeDefaultOrderedByNameWith (tsvOptions True) dumpNutrientTree

allTreeDisplayOpts :: MonadUnliftIO m => TreeExportOptions -> m AllTreeDisplayOptions
allTreeDisplayOpts
  TreeExportOptions
    { treeDisplay
    , treeCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoPrefix
        , ceoMealplan = MealplanOptions {moRoundDigits}
        }
    } = do
    p <- mapM parseCLIPrefixIO ceoPrefix
    return $ AllTreeDisplayOptions treeDisplay ceoShowUnknowns p moRoundDigits

allTabularDisplayOpts
  :: MonadUnliftIO m
  => [SortKey]
  -> TabularExportOptions
  -> m AllTabularDisplayOptions
allTabularDisplayOpts
  ks
  TabularExportOptions
    { tabCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoPrefix
        , ceoMealplan = MealplanOptions {moRoundDigits}
        }
    } = do
    p <- mapM parseCLIPrefixIO ceoPrefix
    return $ AllTabularDisplayOptions ceoShowUnknowns p moRoundDigits ks

parseCLIPrefixIO :: MonadUnliftIO m => Text -> m Prefix
parseCLIPrefixIO s =
  maybe (throwAppErrorIO $ PrefixError s) return $ parseCLIPrefix s

tsvOptions :: Bool -> C.EncodeOptions
tsvOptions h =
  C.defaultEncodeOptions
    { C.encDelimiter = fromIntegral (ord '\t')
    , C.encIncludeHeader = h
    }

parseSortKeysIO :: MonadUnliftIO m => Text -> m [SortKey]
parseSortKeysIO s =
  maybe (throwAppErrorIO (SortKeys s)) pure $ parseSortKeys s

parseSortKeys :: Text -> Maybe [SortKey]
parseSortKeys "" = Just []
parseSortKeys s = fmap L.nub $ mapM parseSortKey $ T.split (== ',') s

parseSortKey :: Text -> Maybe SortKey
parseSortKey = go <=< T.uncons
  where
    go (p, rest) = do
      a <- case p of
        '+' -> pure True
        '-' -> pure False
        _ -> Nothing
      f <- case rest of
        "date" -> pure SortDate
        "meal" -> pure SortMeal
        "ingredient" -> pure SortIngredient
        "nutrient" -> pure SortNutrient
        "parent" -> pure SortParent
        "value" -> pure SortValue
        _ -> Nothing
      pure $ SortKey f a
