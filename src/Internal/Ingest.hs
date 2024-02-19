module Internal.Ingest
  ( downloadFoodItem
  , getStoreAPIKey
  , fetchFID
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Internal.Nutrients
import Internal.Types.CLI
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import RIO
import RIO.FilePath
import qualified RIO.Map as M
import qualified RIO.Set as S
import qualified RIO.Text as T
import UnliftIO.Directory

downloadFoodItem
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Bool
  -> APIKey
  -> FID
  -> m MappedFoodItem
downloadFoodItem forceAPI k fid = do
  j <- jsonDecodeIO =<< fetchFID forceAPI k fid
  let (warn, removed, mfi) = mapFoodItem $ filterFoodItem j
  mapM_ (logWarn . displayText . fmtWarning fid) warn
  mapM_ (logDebug . displayText . fmtRemoved fid) removed
  return mfi

mapFoodItem :: ParsedFoodItem -> ([NutrientWarning], [RemovedNutrient], MappedFoodItem)
mapFoodItem f@FoodItem {fiFoodNutrients = ns} =
  (warn, remove, f {fiFoodNutrients = M.fromList good})
  where
    (warn, remove) = partitionEithers bad
    (bad, good) = partitionEithers $ fmap go ns
    -- NOTE remove all nutrients with a derivation id 4 or 49 (summed or
    -- calculated) since we do this ourselves in a way that is totally
    -- predictable (and is also hopefully comparable to what the database does)
    -- and also assume that if it doesn't have a derivation id that it is
    -- likely correct, which is probably BS but if we don't assume this we would
    -- lose 30-40% of the database.
    go (FoodNutrient (Just (Nutrient (Just i) (Just n) (Just u))) (Just v) d) =
      case d of
        Just 4 -> Left $ Right $ RemovedNutrient i 4
        Just 49 -> Left $ Right $ RemovedNutrient i 49
        _ -> case parseUnit u of
          Just (Unit p Gram) ->
            Right (i, ValidNutrient (Mass $ raisePower (prefixValue p) $ unMass v) p $ Just n)
          Just _ -> Left $ Left $ NotGram i u
          Nothing -> Left $ Left $ UnknownUnit i u
    go n = Left $ Left $ InvalidNutrient n

filterFoodItem :: ParsedFoodItem -> ParsedFoodItem
filterFoodItem f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = filter go ns}
  where
    go fi = maybe True (\i -> not $ S.member i ignoredNutrients) (nId =<< fnNutrient fi)

jsonDecodeIO :: MonadUnliftIO m => FromJSON a => Text -> m a
jsonDecodeIO = either go return . eitherDecodeStrict . encodeUtf8
  where
    go = throwAppErrorIO . JSONError . BC.pack

fetchFID
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> APIKey
  -> FID
  -> m Text
fetchFID frc k i = do
  e <- fidExists i
  if frc then go else if e then readFID i else go
  where
    go = downloadFID k i

fidPath :: MonadUnliftIO m => FID -> m FilePath
fidPath i = do
  d <- cacheDir
  return $ d </> (show i ++ ".json")

cacheDir :: MonadUnliftIO m => m FilePath
cacheDir = getXdgDirectory XdgCache "womp"

fidExists :: MonadUnliftIO m => FID -> m Bool
fidExists i = doesFileExist =<< fidPath i

downloadFID
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => APIKey
  -> FID
  -> m Text
downloadFID k i = do
  p <- fidPath i
  j <- requestJSON k i
  createWriteFile p j
  return j

-- TODO catch errors here
requestJSON
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => APIKey
  -> FID
  -> m Text
requestJSON k i = do
  logInfo $ displayBytesUtf8 $ encodeUtf8 $ T.append "downloading " (tshow i)
  res <-
    R.runReq R.defaultHttpConfig $
      R.req R.GET url R.NoReqBody R.bsResponse opts
  logInfo $ displayBytesUtf8 $ encodeUtf8 $ T.append "downloaded " (tshow i)
  return $ decodeUtf8Lenient $ R.responseBody res
  where
    url = apiFoodURL /~ tshow i
    opts = R.header "X-Api-Key" $ encodeUtf8 $ unAPIKey k

-- | Read FID JSON file
-- ASSUME it has already been downloaded or this will error
readFID :: MonadUnliftIO m => FID -> m Text
readFID i = readFileUtf8 =<< fidPath i

apiFoodURL :: R.Url 'R.Https
apiFoodURL =
  R.https "api.nal.usda.gov"
    /: "fdc"
    /~ ("v1" :: Text)
    /~ ("food" :: Text)

fmtRemoved :: FID -> RemovedNutrient -> Text
fmtRemoved fi (RemovedNutrient ni di) =
  T.unwords
    [ "Removed nutrient"
    , tshow ni
    , "with derivation id"
    , tshow di
    , "from food with id"
    , tshow fi
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

getStoreAPIKey
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Maybe APIKey
  -> m APIKey
getStoreAPIKey k = do
  f <- (</> apiKeyFile) <$> configDir
  case k of
    Just k' -> do
      logDebug $ displayText $ T.append "writing api key to " $ T.pack f
      createWriteFile f (unAPIKey k')
      return k'
    Nothing -> do
      e <- doesFileExist f
      if e then go f else throwAppErrorIO $ MissingAPIKey f
  where
    go f = do
      logDebug $ displayText $ T.append "reading api key from " $ T.pack f
      APIKey <$> readFileUtf8 f

apiKeyFile :: FilePath
apiKeyFile = "apikey"

configDir :: MonadUnliftIO m => m FilePath
configDir = getXdgDirectory XdgConfig "womp"

--------------------------------------------------------------------------------
-- misc types

-- TODO add name to this so that the user is less confused
data NutrientWarning
  = NotGram !NID !Text
  | UnknownUnit !NID !Text
  | InvalidNutrient !FoodNutrient

data RemovedNutrient = RemovedNutrient !NID !DID
