module Internal.Nutrient where

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
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import RIO.State
import qualified RIO.Text as T
import UnliftIO.Directory

-- -- TODO also not DRY
-- customToTree
--   :: [Modification]
--   -> Mass
--   -> CustomSource
--   -> Either CustomIngError (FinalScalerFood, NutrientMap)
-- customToTree ms mass s = do
--   i <- customToItem s
--   return $ first (fmap (scaleNV scale)) $ customItemToTree (foldr modifyItem i ms)
--   where
--     scale = mass / 100

-- -- TODO not dry
-- customItemToTree :: FoodItem () NutrientMap -> (FinalScalerFood, NutrientMap)
-- customItemToTree (FoodItem _ d ns cc pc) =
--   first go $ runState (displayTree pc) ns
--   where
--     go t = fmap toNV $ FinalFood t $ computeCalories cc t
--     toNV v = NutrientValue (Sum v) $ pure $ FoodMeta d Nothing

fromCustomMap :: MonadUnliftIO m => CustomMap -> m (Map Text MappedFoodItem)
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
      remNut = ValidNutrient (standardMass - nutMass) scRemainderPrefix
      remId = NID scRemainder
      dups = findDups $ remId : fmap fst nuts
      nutMap = M.fromList $ (remId, remNut) : nuts
      go CustomNutrient {cnID, cnMass, cnPrefix} =
        ( NID cnID
        , ValidNutrient (Mass $ fromFloatDigits cnMass) cnPrefix
        )

-- TODO this should actually have the meal name as well, which would restrict
-- each ingredient to only be used once in a given meal (which seems like a
-- sane enforcement)
type IngMeta = (MealGroup, Mass, [Modification])

flattenNonEmpty :: NonEmpty (a, NonEmpty b) -> NonEmpty (a, b)
flattenNonEmpty = sconcat . fmap (\(x, ys) -> (x,) <$> ys)

readMealPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> m (NonEmpty ValidSchedule)
readMealPlan f = do
  logDebug $ displayText $ T.append "reading schedule at path: " $ T.pack f
  ss <-
    liftIO $
      if isDhall f
        then D.inputFile D.auto f
        else
          if isYaml f
            then Y.decodeFileThrow f
            else throwAppErrorIO $ FileTypeError f
  vs <- mapM checkSched ss
  maybeExit "meal plan is empty" $ N.nonEmpty vs

-- TODO make sure all masses are positive
checkSched :: MonadUnliftIO m => Schedule -> m ValidSchedule
checkSched Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} = do
  fromEither $ checkCronPat schWhen
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  return $ ValidSchedule is (MealGroup mlName) schWhen $ fromFloatDigits $ fromMaybe 1.0 schScale

expandSchedule
  :: NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> [(Either FID Text, IngredientMetadata)]
expandSchedule vs = sconcat . N.zipWith expandValid vs

expandScheduleIO
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> m (NonEmpty (Either FID Text, IngredientMetadata))
expandScheduleIO vs = maybeExit msg . N.nonEmpty . expandSchedule vs
  where
    msg = "Schedule does not intersect with desired date range"

exitError :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Text -> m a
exitError msg = logError (displayText msg) >> exitFailure

maybeExit
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Text
  -> Maybe a
  -> m a
maybeExit msg = maybe (exitError msg) return

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
  (fromSource ingSource, IngredientMetadata mg mass ingModifications ds)
  where
    mass = Mass $ s * fromFloatDigits ingMass
    fromSource (FDC i) = Left $ FID i
    fromSource (Custom c) = Right c

isDhall :: FilePath -> Bool
isDhall = isExtensionOf "dhall"

isYaml :: FilePath -> Bool
isYaml f = isExtensionOf "yaml" f || isExtensionOf "yml" f

-- TODO use reader for key and map?
getIngredients
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => APIKey
  -> Map Text MappedFoodItem
  -> NonEmpty (Either FID Text, a)
  -> m (NonEmpty (MappedFoodItem, a))
getIngredients k cm = mapM (\(x, y) -> (,y) <$> either fromFDC fromCustom x)
  where
    -- TODO do I want to do all this here?
    fromFDC fid = do
      j <- jsonDecodeIO =<< getFoodJSON k fid
      let (ws, mfi) = mapFoodItem $ filterFoodItem j
      mapM_ (logWarn . displayText . fmtWarning fid) ws
      return mfi
    fromCustom n = maybe (throwAppErrorIO $ MissingCustom n) return $ M.lookup n cm

expandIngredientTrees
  :: NonEmpty (MappedFoodItem, IngredientMetadata)
  -> (NonEmpty (DisplayTree GroupByAll), [UnusedNutrient])
expandIngredientTrees =
  second sconcat . N.unzip . fmap (uncurry (flip ingredientToTree))

ingredientToTree
  :: IngredientMetadata
  -> MappedFoodItem
  -> (DisplayTree GroupByAll, [UnusedNutrient])
ingredientToTree
  IngredientMetadata {imMeal, imMass, imMods, imDaySpan}
  (FoodItem desc ns cc pc) =
    bimap go nutMapToUnused $
      runState (displayTree pc) $
        foldr modifyMap ns imMods
    where
      g = GroupVars imDaySpan imMeal $ IngredientGroup desc
      scale = (* (unMass imMass / 100))
      go t =
        bimap (Mass . scale . unMass) (Energy . scale . unEnergy) $
          DisplayTree_ t (computeCalories cc t) g

nutMapToUnused :: NutrientMap -> [UnusedNutrient]
nutMapToUnused = fmap (uncurry UnusedNutrient) . M.toList

mapFoodItem :: ParsedFoodItem -> ([NutrientWarning], MappedFoodItem)
mapFoodItem f@FoodItem {fiFoodNutrients = ns} =
  second (\ns' -> f {fiFoodNutrients = M.fromList ns'}) $
    partitionEithers $
      fmap go ns
  where
    go (FoodNutrient (Just (Nutrient (Just i) (Just _) (Just u))) (Just v)) =
      case parseUnit u of
        Just (Unit p Gram) ->
          Right (i, ValidNutrient (Mass $ raisePower (prefixValue p) $ unMass v) p)
        Just _ -> Left $ NotGram i u
        Nothing -> Left $ UnknownUnit i u
    go n = Left $ InvalidNutrient n

filterFoodItem :: ParsedFoodItem -> ParsedFoodItem
filterFoodItem f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = filter go ns}
  where
    go fi = maybe True (\i -> not $ S.member i ignoredNutrients) (nId =<< fnNutrient fi)

-- TODO warn user when modifications don't match
-- modifyItem :: Modification -> MappedFoodItem -> MappedFoodItem
-- modifyItem m f = f {fiFoodNutrients = modifyMap m $ fiFoodNutrients f}

modifyMap :: Modification -> NutrientMap -> NutrientMap
modifyMap Modification {modNutID, modScale} = M.adjust go (fromIntegral modNutID)
  where
    go n@ValidNutrient {vnAmount} =
      n {vnAmount = vnAmount * Mass (fromFloatDigits modScale)}

-- TODO use desc somewhere
-- foodItemToTree :: MappedFoodItem -> (DisplayTree (), NutrientMap)
-- foodItemToTree (FoodItem _ ns cc pc) = first go $ runState (displayTree pc) ns
--   where
--     go t = DisplayTree_ t (computeCalories cc t) ()

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

createWriteFile :: MonadUnliftIO m => FilePath -> Text -> m ()
createWriteFile p t = do
  createDirectoryIfMissing True $ takeDirectory p
  writeFileUtf8 p t

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

displayText :: Text -> Utf8Builder
displayText = displayBytesUtf8 . encodeUtf8

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
