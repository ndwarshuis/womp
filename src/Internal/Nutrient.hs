module Internal.Nutrient
  ( readDisplayTrees
  , fetchFID
  , readSummary
  )
where

import Data.Aeson
import Data.Bitraversable
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
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import RIO.State
import qualified RIO.Text as T
import UnliftIO.Directory

-- TODO we don't need an API key if we only have custom nutrients (rare but
-- possible)
readDisplayTrees
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> APIKey
  -> NonEmpty DaySpan
  -> FilePath
  -> m (NonEmpty (DisplayTree GroupByAll))
readDisplayTrees forceAPI k ds p = do
  (vs, customMap) <- readPlan p
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
  is <- expandScheduleIO vs ds
  xs <- mapPooledErrorsIO (bimapM (getIngredients forceAPI k customMap) return) is
  let (ts, unused) = expandIngredientTrees xs
  mapM_ (logWarn . displayText . fmtUnused) unused
  return ts

readSummary
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => Bool
  -> APIKey
  -> NonEmpty DaySpan
  -> FilePath
  -> m (NonEmpty SummaryRow)
readSummary forceAPI k ds p = do
  (vs, customMap) <- readPlan p
  is <- expandPlanIO vs ds
  xs <- mapPooledErrorsIO (bimapM (getIngredients forceAPI k customMap) return) is
  return $ fmap (uncurry expandIngredientSummary) xs

readPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> m (NonEmpty ValidSchedule, ValidCustomMap)
readPlan f = do
  logDebug $ displayText $ T.append "reading schedule at path: " $ T.pack f
  p <-
    liftIO $
      if isDhall f
        then D.inputFile D.auto f
        else
          if isYaml f
            then Y.decodeFileThrow f
            else throwAppErrorIO $ FileTypeError f
  ss <- mapM checkSched $ schedule p
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
checkSched :: MonadUnliftIO m => Schedule -> m ValidSchedule
checkSched Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} = do
  fromEither $ checkCronPat schWhen
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  return $ ValidSchedule is (MealGroup mlName) schWhen $ fromFloatDigits $ fromMaybe 1.0 schScale

-- TODO terrible name
expandPlanIO
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> m (NonEmpty (Either FID Text, IngredientMealMeta))
expandPlanIO vs = maybeExit msg . N.nonEmpty . expandPlan vs
  where
    msg = "Schedule does not intersect with desired date range"

-- TODO not dry
expandPlan
  :: NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> [(Either FID Text, IngredientMealMeta)]
expandPlan vs = sconcat . N.zipWith expandValidToSummary vs

expandValidToSummary
  :: ValidSchedule
  -> DaySpan
  -> [(Either FID Text, IngredientMealMeta)]
expandValidToSummary ValidSchedule {vsIngs, vsMeal, vsScale, vsCron} ds =
  [go i d | i <- N.toList vsIngs, d <- expandCronPat ds vsCron]
  where
    go (Ingredient {ingMass, ingSource}) d =
      ( sourceToEither ingSource
      , IngredientMetadata_ vsMeal (Mass $ (vsScale * fromFloatDigits ingMass)) () d
      )

expandScheduleIO
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> m (NonEmpty (Either FID Text, IngredientMetadata))
expandScheduleIO vs = maybeExit msg . N.nonEmpty . expandSchedule vs
  where
    msg = "Schedule does not intersect with desired date range"

expandSchedule
  :: NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> [(Either FID Text, IngredientMetadata)]
expandSchedule vs = sconcat . N.zipWith expandValid vs

expandValid :: ValidSchedule -> DaySpan -> [(Either FID Text, IngredientMetadata)]
expandValid ValidSchedule {vsIngs, vsMeal, vsCron, vsScale} ds
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

-- TODO use reader for key and map?
getIngredients
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Bool
  -> APIKey
  -> ValidCustomMap
  -> Either FID Text
  -> m MappedFoodItem
getIngredients forceAPI k cm = either fromFDC fromCustom
  where
    -- TODO do I want to do all this here?
    fromFDC fid = do
      j <- jsonDecodeIO =<< fetchFID forceAPI k fid
      let (ws, mfi) = mapFoodItem $ filterFoodItem j
      mapM_ (logWarn . displayText . fmtWarning fid) ws
      return mfi
    fromCustom n = maybe (throwAppErrorIO $ MissingCustom n) return $ M.lookup n cm

expandIngredientTrees
  :: NonEmpty (MappedFoodItem, IngredientMetadata)
  -> (NonEmpty (DisplayTree GroupByAll), [UnusedNutrient])
expandIngredientTrees =
  second sconcat . N.unzip . fmap (uncurry (flip ingredientToTree))

expandIngredientSummary :: MappedFoodItem -> IngredientMealMeta -> SummaryRow
expandIngredientSummary
  FoodItem {fiDescription}
  IngredientMetadata_ {imMeal, imMass, imDaySpan} =
    SummaryRow imDaySpan imMeal (IngredientGroup fiDescription) imMass

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
