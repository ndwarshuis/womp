module Internal.Nutrient
  ( readDisplayTrees
  , fetchFID
  , readSummary
  , getStoreAPIKey
  , parseSortKeys
  )
where

import Data.Aeson
import qualified Data.ByteString.Char8 as BC
import Data.Scientific
import Data.Semigroup
import qualified Data.Yaml as Y
import qualified Dhall as D
import Internal.NutrientTree
import Internal.Nutrients
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import Network.HTTP.Req ((/:), (/~))
import qualified Network.HTTP.Req as R
import RIO
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import RIO.State
import qualified RIO.Text as T
import UnliftIO.Directory

readDisplayTrees
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> Maybe APIKey
  -> NonEmpty DaySpan
  -> FilePath
  -> Int
  -> m (NonEmpty (DisplayTree GroupByAll))
readDisplayTrees frc k ds p norm = do
  is <- readMappedItems validToIngredient frc k ds p norm
  let (ts, unused) = expandIngredientTrees is
  -- TODO nub hack to deal with repeated warnings
  mapM_ (logWarn . displayText . fmtUnused) $ L.nub unused
  return ts

readSummary
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> Maybe APIKey
  -> NonEmpty DaySpan
  -> FilePath
  -> Int
  -> Int
  -> m (NonEmpty SummaryRow)
readSummary frc k ds p norm r = do
  is <- readMappedItems validToSummary frc k ds p norm
  return $ fmap (uncurry (expandIngredientSummary r)) is

readMappedItems
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => (ValidSchedule -> DaySpan -> [(Either FID Text, a)])
  -> Bool
  -> Maybe APIKey
  -> NonEmpty DaySpan
  -> FilePath
  -> Int
  -> m (NonEmpty (MappedFoodItem, a))
readMappedItems f forceAPI k ds p norm = do
  (vs, customMap) <- readPlan p norm
  is <- expandSchedule f vs ds
  let (fs, cs) = N.unzip $ groupByTup is
  ms <- mapFIDs k (fromFDC forceAPI) fs
  fs' <- mapM (either return (fromCustom customMap)) ms
  return $ flatten $ N.zip fs' cs

groupByTup :: Eq a => NonEmpty (a, b) -> NonEmpty (a, NonEmpty b)
groupByTup = fmap (\xs -> (fst $ N.head xs, snd <$> xs)) . N.groupWith1 fst

flatten :: NonEmpty (a, NonEmpty b) -> NonEmpty (a, b)
flatten = sconcat . fmap go
  where
    go (x, ys) = (x,) <$> ys

-- | Map over a non-empty list which contains no/some FIDs
-- If the list has no FIDs, don't ask for an API key (which will fail if
-- not available). This is useful for cases where none of the ingredients in
-- the plan require an API lookup, in which case the program will not fail if
-- an API key is not provided.
mapFIDs
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Maybe APIKey
  -> (APIKey -> FID -> m a)
  -> NonEmpty (Either FID b)
  -> m (NonEmpty (Either a b))
mapFIDs k f (x :| xs) = case x of
  Left y -> (`append` zs') <$> go (y :| ys)
  Right y ->
    let rs = Right y :| zs'
     in maybe (pure rs) (fmap (append rs . N.toList) . go) $ N.nonEmpty ys
  where
    (ys, zs) = partitionEithers xs
    zs' = Right <$> zs
    go = fmap (fmap Left) . mapWithAPIKey k f

-- | Transform a non-empty list of FIDs with an API key
mapWithAPIKey
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Maybe APIKey
  -> (APIKey -> FID -> m a)
  -> NonEmpty FID
  -> m (NonEmpty a)
mapWithAPIKey k f xs = do
  k' <- getStoreAPIKey k
  mapPooledErrorsIO (f k') xs

readPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> Int
  -> m (NonEmpty ValidSchedule, ValidCustomMap)
readPlan f norm = do
  -- TODO don't exit here, throw and exception so that I can catch multiple
  -- issues at once and alert the user
  when (norm < 1) $ exitError "normalization constant must be 1 or more"
  logDebug $ displayText $ T.append "reading schedule at path: " $ T.pack f
  p <-
    liftIO $
      if isDhall f
        then D.inputFile D.auto f
        else
          if isYaml f
            then Y.decodeFileThrow f
            else throwAppErrorIO $ FileTypeError f
  ss <- mapM (`checkSched` norm) $ schedule p
  vs <- maybeExit "meal plan is empty" $ N.nonEmpty ss
  cm <- fromCustomMap $ customIngredients p
  return (vs, cm)

fromCustomMap :: MonadUnliftIO m => CustomMap -> m ValidCustomMap
fromCustomMap =
  mapM (either (throwAppErrorIO . CustomIngError) return . customToItem)

customToItem :: CustomIngredient -> Either CustomIngError MappedFoodItem
customToItem
  CustomIngredient
    { scDesc
    , scRemainder
    , scRemainderPrefix
    , scNutrients
    , scCalorie
    , scProtein
    } =
    case N.nonEmpty dups of
      Just ds -> Left $ CustomDups scDesc ds
      Nothing
        | nutMass > standardMass -> Left $ TooMuchMass scDesc
        | otherwise -> return $ FoodItem scDesc nutMap scCalorie pc
    where
      pc = ProteinConversion $ fromFloatDigits scProtein
      nuts = go <$> scNutrients
      nutMass = sum $ vnAmount . snd <$> nuts
      remNut = ValidNutrient (standardMass - nutMass) scRemainderPrefix Nothing
      remId = NID scRemainder
      dups = findDups $ remId : fmap fst nuts
      nutMap = M.fromList $ (remId, remNut) : nuts
      go CustomNutrient {cnID, cnMass, cnPrefix} =
        ( NID cnID
        , ValidNutrient (Mass $ fromFloatDigits cnMass) cnPrefix Nothing
        )

-- TODO make sure all masses are positive
checkSched :: MonadUnliftIO m => Schedule -> Int -> m ValidSchedule
checkSched Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} norm = do
  fromEither $ checkCronPat schWhen
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  return $
    ValidSchedule is (MealGroup mlName) schWhen $
      fromFloatDigits (fromMaybe 1.0 schScale / fromIntegral norm)

expandSchedule
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => (ValidSchedule -> DaySpan -> [(Either FID Text, a)])
  -> NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> m (NonEmpty (Either FID Text, a))
expandSchedule f vs = maybeExit msg . N.nonEmpty . go
  where
    msg = "Schedule does not intersect with desired date range"
    go = sconcat . nonEmptyProduct f vs

nonEmptyProduct :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
nonEmptyProduct f xs = sconcat . fmap (\y -> fmap (`f` y) xs)

validToSummary
  :: ValidSchedule
  -> DaySpan
  -> [(Either FID Text, IngredientMealMeta)]
validToSummary ValidSchedule {vsIngs, vsMeal, vsScale, vsCron} ds =
  [go i d | i <- N.toList vsIngs, d <- expandCronPat ds vsCron]
  where
    go Ingredient {ingMass, ingSource} d =
      ( sourceToEither ingSource
      , IngredientMetadata_ vsMeal (Mass (vsScale * fromFloatDigits ingMass)) () d
      )

validToIngredient :: ValidSchedule -> DaySpan -> [(Either FID Text, IngredientMetadata)]
validToIngredient ValidSchedule {vsIngs, vsMeal, vsCron, vsScale} ds
  | d == 0 = []
  | otherwise = N.toList $ fmap (expandIngredient vsMeal (d * vsScale) ds) vsIngs
  where
    d = fromIntegral $ length $ expandCronPat ds vsCron

expandIngredient
  :: MealGroup
  -> Scientific
  -> DaySpan
  -> Ingredient
  -> (Either FID Text, IngredientMetadata)
expandIngredient mg s ds Ingredient {ingMass, ingModifications, ingSource} =
  (sourceToEither ingSource, IngredientMetadata_ mg mass ingModifications ds)
  where
    mass = Mass $ s * fromFloatDigits ingMass

sourceToEither :: IngredientSource -> Either FID Text
sourceToEither (FDC i) = Left $ FID i
sourceToEither (Custom c) = Right c

isDhall :: FilePath -> Bool
isDhall = isExtensionOf "dhall"

isYaml :: FilePath -> Bool
isYaml f = isExtensionOf "yaml" f || isExtensionOf "yml" f

fromFDC
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Bool
  -> APIKey
  -> FID
  -> m MappedFoodItem
fromFDC forceAPI k fid = do
  j <- jsonDecodeIO =<< fetchFID forceAPI k fid
  let (ws, mfi) = mapFoodItem $ filterFoodItem j
  mapM_ (logWarn . displayText . fmtWarning fid) ws
  return mfi

fromCustom :: MonadUnliftIO m => ValidCustomMap -> Text -> m MappedFoodItem
fromCustom cm n = maybe (throwAppErrorIO $ MissingCustom n) return $ M.lookup n cm

-- TODO this is run after the nonempty tree is flattened and thus will print
-- warnings for each group combination, which is super annoying
expandIngredientTrees
  :: NonEmpty (MappedFoodItem, IngredientMetadata)
  -> (NonEmpty (DisplayTree GroupByAll), [UnusedNutrient])
expandIngredientTrees =
  second sconcat . N.unzip . fmap (uncurry (flip ingredientToTree))

expandIngredientSummary :: Int -> MappedFoodItem -> IngredientMealMeta -> SummaryRow
expandIngredientSummary
  r
  FoodItem {fiDescription}
  IngredientMetadata_ {imMeal, imMass, imDaySpan} =
    SummaryRow imDaySpan imMeal (IngredientGroup fiDescription) (roundDigits r imMass)

ingredientToTree
  :: IngredientMetadata
  -> MappedFoodItem
  -> (DisplayTree GroupByAll, [UnusedNutrient])
ingredientToTree
  IngredientMetadata_ {imMeal, imMass, imMods, imDaySpan}
  (FoodItem desc ns cc pc) =
    bimap go (nutMapToUnused imMeal) $
      runState (displayTree pc) $
        foldr modifyMap ns imMods
    where
      g = GroupVars imDaySpan imMeal $ IngredientGroup desc
      scale = (* (unMass imMass / 100))
      go t =
        bimap (Mass . scale . unMass) (Energy . scale . unEnergy) $
          DisplayTree_ t (computeCalories cc t) g

nutMapToUnused :: MealGroup -> NutrientMap -> [UnusedNutrient]
nutMapToUnused m = fmap (uncurry (UnusedNutrient m)) . M.toList

mapFoodItem :: ParsedFoodItem -> ([NutrientWarning], MappedFoodItem)
mapFoodItem f@FoodItem {fiFoodNutrients = ns} =
  second (\ns' -> f {fiFoodNutrients = M.fromList ns'}) $
    partitionEithers $
      fmap go ns
  where
    go (FoodNutrient (Just (Nutrient (Just i) (Just n) (Just u))) (Just v)) =
      case parseUnit u of
        Just (Unit p Gram) ->
          Right (i, ValidNutrient (Mass $ raisePower (prefixValue p) $ unMass v) p $ Just n)
        Just _ -> Left $ NotGram i u
        Nothing -> Left $ UnknownUnit i u
    go n = Left $ InvalidNutrient n

filterFoodItem :: ParsedFoodItem -> ParsedFoodItem
filterFoodItem f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = filter go ns}
  where
    go fi = maybe True (\i -> not $ S.member i ignoredNutrients) (nId =<< fnNutrient fi)

-- TODO warn user when modifications don't match
modifyMap :: Modification -> NutrientMap -> NutrientMap
modifyMap Modification {modNutID, modScale} = M.adjust go (fromIntegral modNutID)
  where
    go n@ValidNutrient {vnAmount} =
      n {vnAmount = vnAmount * Mass (fromFloatDigits modScale)}

computeCalories :: CalorieConversion -> DisplayNode Mass -> Energy
computeCalories (CalorieConversion ff pf cf) dn =
  Energy $
    fromFloatDigits ff * go (measToDisplay lipid)
      + fromFloatDigits pf * go dispProtein
      + fromFloatDigits cf * go (summedToDisplay carbDiff)
  where
    go n = unMass $ fromMaybe 0 $ lookupTree n dn

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
  j <- getFoodJSON k i
  createWriteFile p j
  return j

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

fmtUnused :: UnusedNutrient -> T.Text
fmtUnused (UnusedNutrient m i n) =
  T.concat
    [ "Unused nutrient with id "
    , tshow i
    , T.append " in meal " $ tshow m
    , ": "
    , tshow n
    ]

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
