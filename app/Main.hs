module Main (main) where

import qualified Data.Aeson as A
import Data.Char (isDigit, ord)
import qualified Data.Csv as C
import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
import Internal.CLI
import Internal.Display
import Internal.Export
import Internal.Ingest
import Internal.Types.CLI
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import Options.Applicative
import RIO
import qualified RIO.ByteString as B
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.NonEmpty as N
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP

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
    level 0 = LevelError
    level 1 = LevelWarn
    level 2 = LevelInfo
    level _ = LevelDebug

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
        treeToCSV opts (tsvOptions tabHeader) (ceoGroup tabCommonExport) $
          N.toList ts
  where
    readTrees = readDisplayTrees (ceoMealplan tabCommonExport)
    filt = ceoFilter tabCommonExport
    getOpts = allTabularDisplayOpts tabSort filt tos

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
  combineErrorIOM2 readTrees (allTreeDisplayOpts filt t) $ \ts atds ->
    liftIO $ go $ treeToJSON atds gos $ N.toList ts
  where
    readTrees = readDisplayTrees $ ceoMealplan treeCommonExport
    gos = ceoGroup treeCommonExport
    filt = ceoFilter treeCommonExport
    go = if treeJSON then BL.putStr . A.encode else B.putStr . Y.encode

runListNutrients :: MonadUnliftIO m => m ()
runListNutrients =
  BL.putStr $ C.encodeDefaultOrderedByNameWith (tsvOptions True) dumpNutrientTree

allTreeDisplayOpts
  :: MonadUnliftIO m
  => Text
  -> TreeExportOptions
  -> m AllTreeDisplayOptions
allTreeDisplayOpts
  filt
  TreeExportOptions
    { treeDisplay
    , treeCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoPrefix
        , ceoMealplan = MealplanOptions {moRoundDigits}
        , ceoEnergy
        }
    } = do
    (p, fs) <-
      combineErrorIO2
        (mapM parseCLIPrefixIO ceoPrefix)
        (parseFilterKeysIO filt)
        (,)
    return $ AllTreeDisplayOptions treeDisplay ceoShowUnknowns p moRoundDigits fs (not ceoEnergy)

allTabularDisplayOpts
  :: MonadUnliftIO m
  => Text
  -> Text
  -> TabularExportOptions
  -> m AllTabularDisplayOptions
allTabularDisplayOpts
  srt
  filt
  TabularExportOptions
    { tabCommonExport =
      CommonExportOptions
        { ceoShowUnknowns
        , ceoPrefix
        , ceoMealplan = MealplanOptions {moRoundDigits}
        , ceoEnergy
        }
    } = do
    (p, ss, fs) <-
      combineErrorIO3
        (mapM parseCLIPrefixIO ceoPrefix)
        (parseSortKeysIO srt)
        (parseFilterKeysIO filt)
        (,,)
    return $ AllTabularDisplayOptions ceoShowUnknowns p moRoundDigits ss fs (not ceoEnergy)

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

parseFilterKeysIO :: MonadUnliftIO m => Text -> m [FilterKey]
parseFilterKeysIO s =
  maybe (throwAppErrorIO (FilterKeys s)) pure $ parseFilterKeys s

parseSortKeys :: Text -> Maybe [SortKey]
parseSortKeys = parseArgList parseSortKey

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

parseFilterKeys :: Text -> Maybe [FilterKey]
parseFilterKeys = parseArgList parseFilterKey

parseFilterKey :: Text -> Maybe FilterKey
parseFilterKey s = do
  (p, rest) <- T.uncons s
  case p of
    '!' -> go False rest
    _ -> go True s
  where
    go keep kv =
      foldr (<|>) (parseRegexpFilters keep kv) $
        fmap (uncurry (parseValueFilter keep kv)) opPairs

    parseRegexpFilters keep kv = do
      (k, re) <- splitMaybe "~" kv
      let g = GroupFilter . GroupFilterKey keep re
      case k of
        "meal" -> Just $ g FilterMeal
        "ingredient" -> Just $ g FilterIngredient
        "nutrient" -> Just $ TreeFilter $ TreeFilterKey keep $ FilterNutrient re
        _ -> Nothing

    parseValueFilter keep kv opChar op = do
      (k, v) <- splitMaybe opChar kv
      v' <- case k of
        "value" -> do
          let (d, p) = T.span (\x -> isDigit x || x == '.') v
          d' <- readMaybe $ T.unpack d
          p' <- case p of
            "" -> pure Unity
            p' -> parseCLIPrefix p'
          return $ toUnity p' d'
        _ -> Nothing
      return $ TreeFilter $ TreeFilterKey keep $ FilterValue (Mass v') op

    opPairs =
      [ ("=", EQ_)
      , ("<", LT_)
      , (">", GT_)
      , ("<=", LTE_)
      , (">=", GTE_)
      ]

splitMaybe :: Text -> Text -> Maybe (Text, Text)
splitMaybe c s = case TP.splitOn c s of
  [x, y] -> Just (x, y)
  _ -> Nothing

parseArgList :: Eq a => (Text -> Maybe a) -> Text -> Maybe [a]
parseArgList _ "" = Just []
parseArgList f s = fmap L.nub $ mapM f $ T.split (== ',') s
