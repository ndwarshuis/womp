module Internal.Export
  ( readDisplayTrees
  , readSummary
  )
where

import Data.Scientific
import Data.Semigroup
-- import qualified Data.Text.IO as TI
import qualified Data.Yaml as Y
import qualified Dhall as D
import Internal.Ingest
import Internal.Nutrients
import Internal.Types.CLI
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import RIO.State
import qualified RIO.Text as T
import RIO.Time

--------------------------------------------------------------------------------
-- export meal plan to tree or table

readDisplayTrees
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => MealplanOptions
  -> m DisplayTrees
readDisplayTrees mos = do
  is <- readMappedItems validToExport mos
  let (ts, unused) = expandIngredientTrees is
  -- TODO nub hack to deal with repeated warnings
  mapM_ (logWarn . displayText . fmtUnused) $ L.nub unused
  return ts

readSummary
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => MealplanOptions
  -> m (NonEmpty SummaryRow)
readSummary mos@MealplanOptions {moRoundDigits} = do
  is <- readMappedItems validToSummary mos
  return $ fmap (uncurry go) is
  where
    go FoodItem {fiDescription} IngredientMetadata {imMeal, imMass, imDaySpan} =
      SummaryRow
        imDaySpan
        imMeal
        (IngredientGroup fiDescription)
        (roundDigits moRoundDigits imMass)

readMappedItems
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => (ValidSchedule -> DaySpan -> [(Source, a)])
  -> MealplanOptions
  -> m (NonEmpty (MappedFoodItem, a))
readMappedItems f MealplanOptions {moForce, moMealPath, moDateInterval, moThreads, moKey} = do
  setThreads moThreads
  (ds, norm) <- dateIntervalToDaySpan moDateInterval
  (vs, customMap) <- readPlan moMealPath norm
  is <- expandSchedule f vs ds
  -- liftIO $ TI.putStr $ tshow is
  let (fs, cs) = N.unzip $ groupByTup is
  ms <- mapFIDs moKey (downloadFoodItem moForce) fs
  fs' <- mapM (either return (fromCustom customMap)) ms
  return $ flatten $ N.zip fs' cs

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
mapFIDs k f (x :| xs) =
  fmap snd . N.sortWith fst <$> case x of
    Left y -> (`append` zs) <$> go ((0, y) :| ys)
    Right y ->
      let rs = (0, Right y) :| zs
       in maybe (pure rs) (fmap (append rs . N.toList) . go) $ N.nonEmpty ys
  where
    (ys, zs) =
      partitionEithers $
        L.zipWith (\i -> bimap (i,) ((i,) . Right)) [(1 :: Int) ..] xs
    go as =
      let (is, bs) = N.unzip as
       in N.zip is . fmap Left <$> mapWithAPIKey k f bs

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

--------------------------------------------------------------------------------
-- date interval -> list day span

dateIntervalToDaySpan
  :: MonadUnliftIO m
  => DateIntervalOptions
  -> m (NonEmpty DaySpan, Int)
dateIntervalToDaySpan
  DateIntervalOptions
    { dioStart
    , dioEnd
    , dioDays
    , dioInterval
    , dioNormalize
    } = do
    start <- maybe currentDay return dioStart
    combineErrorIO3 (getLen start) checkInterval (checkNormalize dioNormalize) $
      \totalLen _ _ -> (toDaySpans dioInterval start totalLen, dioNormalize)
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

checkNormalize :: MonadUnliftIO m => Int -> m ()
checkNormalize x = when (x < 1) $ throwAppErrorIO (NormalizeError x)

toDaySpans :: Maybe Int -> Day -> Int -> NonEmpty DaySpan
toDaySpans interval start totalLen = case interval of
  Just i
    | i >= totalLen -> span1
    | otherwise ->
        let (n, r) = divMod totalLen i
            lastStart = addDays (fromIntegral $ n * i) start
            last = if r == 0 then [] else N.toList $ genSpans 1 r lastStart
         in append (genSpans n i start) last
  Nothing -> span1
  where
    span1 = genSpans 1 totalLen start
    genSpans n s = take1 n . fmap (,s - 1) . N.iterate (addDays $ fromIntegral s)

currentDay :: MonadUnliftIO m => m Day
currentDay = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  return $ localDay $ utcToLocalTime z u

--------------------------------------------------------------------------------
-- custom map

fromCustom :: MonadUnliftIO m => ValidCustomMap -> CustomID -> m MappedFoodItem
fromCustom cm n = maybe (throwAppErrorIO $ MissingCustom n) return $ M.lookup n cm

fromCustomMap :: MonadUnliftIO m => CustomMap -> m ValidCustomMap
fromCustomMap =
  mapM (either (throwAppErrorIO . CustomIngError) return . customToItem)

customToItem :: CustomIngredient -> Either CustomIngError MappedFoodItem
customToItem
  CustomIngredient
    { scDesc
    , scRemainder
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
      remNut = ValidNutrient (standardMass - nutMass) Unity Nothing
      remId = NID scRemainder
      dups = findDups $ remId : fmap fst nuts
      nutMap = M.fromList $ (remId, remNut) : nuts
      go CustomNutrient {cnID, cnMass} =
        ( NID cnID
        , ValidNutrient (Mass $ fromFloatDigits cnMass) Unity Nothing
        )

--------------------------------------------------------------------------------
-- reading meal plan from disk and validating

readPlan
  :: (MonadReader env m, MonadUnliftIO m, HasLogFunc env)
  => FilePath
  -> Int
  -> m (NonEmpty ValidSchedule, ValidCustomMap)
readPlan f norm = do
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
  vs <- maybe (throwAppErrorIO (EmptySchedule False)) return $ N.nonEmpty ss
  cm <- fromCustomMap $ customIngredients p
  return (vs, cm)
  where
    isDhall = isExtensionOf "dhall"
    isYaml p = isExtensionOf "yaml" p || isExtensionOf "yml" p

checkSched :: MonadUnliftIO m => Schedule -> Int -> m ValidSchedule
checkSched Schedule {schMeal = Meal {mlIngs, mlName}, schWhen, schScale} norm = do
  fromEither $ checkCronPat schWhen
  is <- maybe (throwAppErrorIO $ EmptyMeal mlName) return $ N.nonEmpty mlIngs
  _ <- mapErrorsIO checkIngredient is
  return $
    ValidSchedule is (MealGroup mlName) schWhen $
      fromFloatDigits (fromMaybe 1.0 schScale / fromIntegral norm)

checkIngredient :: MonadUnliftIO m => Ingredient -> m ()
checkIngredient Ingredient {ingMass, ingSource} =
  -- TODO check that modifications are valid
  when (ingMass < 0) $ throwAppErrorIO $ MassError ingSource ingMass

--------------------------------------------------------------------------------
-- "expanding" meal plan
--
-- basically a cartesian product of meals and date ranges

expandSchedule
  :: MonadUnliftIO m
  => (ValidSchedule -> DaySpan -> [(Source, a)])
  -> NonEmpty ValidSchedule
  -> NonEmpty DaySpan
  -> m (NonEmpty (Source, a))
expandSchedule f vs = maybe (throwAppErrorIO (EmptySchedule True)) return . N.nonEmpty . go
  where
    go = sconcat . nonEmptyProduct f vs

validToSummary
  :: ValidSchedule
  -> DaySpan
  -> [(Source, SummaryIngredientMetadata)]
validToSummary ValidSchedule {vsIngs, vsMeal, vsScale, vsCron} ds =
  [go i d | i <- N.toList vsIngs, d <- expandCronPat ds vsCron]
  where
    go Ingredient {ingMass, ingSource} d =
      ( source ingSource
      , IngredientMetadata vsMeal (Mass (vsScale * fromFloatDigits ingMass)) () d
      )

validToExport
  :: ValidSchedule
  -> DaySpan
  -> [(Source, ExportIngredientMetadata)]
validToExport ValidSchedule {vsIngs, vsMeal, vsCron, vsScale} ds
  | d == 0 = []
  | otherwise = N.toList $ fmap (expandIngredient vsMeal (d * vsScale) ds) vsIngs
  where
    d = fromIntegral $ length $ expandCronPat ds vsCron

expandIngredient
  :: MealGroup
  -> Scientific
  -> DaySpan
  -> Ingredient
  -> (Source, ExportIngredientMetadata)
expandIngredient mg s ds Ingredient {ingMass, ingModifications, ingSource} =
  (source ingSource, IngredientMetadata mg mass ingModifications ds)
  where
    mass = Mass $ s * fromFloatDigits ingMass

source :: IngredientSource -> Source
source (FDC i) = Left $ FID i
source (Custom c) = Right c

--------------------------------------------------------------------------------
-- food item -> tree

-- TODO this is run after the nonempty tree is flattened and thus will print
-- warnings for each group combination, which is super annoying
expandIngredientTrees
  :: NonEmpty (MappedFoodItem, ExportIngredientMetadata)
  -> (DisplayTrees, [UnusedNutrient])
expandIngredientTrees =
  second sconcat . N.unzip . fmap (uncurry (flip ingredientToTree))

ingredientToTree
  :: ExportIngredientMetadata
  -> MappedFoodItem
  -> (DisplayTree GroupByAll, [UnusedNutrient])
ingredientToTree
  IngredientMetadata {imMeal, imMass, imMods, imDaySpan}
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

nutMapToUnused :: MealGroup -> NutrientMap -> [UnusedNutrient]
nutMapToUnused m = fmap (uncurry (UnusedNutrient m)) . M.toList

fmtUnused :: UnusedNutrient -> T.Text
fmtUnused (UnusedNutrient m i n) =
  T.concat
    [ "Unused nutrient with id "
    , tshow i
    , T.append " in meal " $ tshow m
    , ": "
    , tshow n
    ]

-- TODO not sure how efficient this is; probably doesn't matter than much ;)
lookupTree :: DisplayNutrient -> DisplayNode a -> Maybe a
lookupTree k (DisplayNode _ ks _) =
  msum ((dnValue <$> M.lookup k ks) : (lookupTree k <$> M.elems ks))

entireTree :: NutrientState m => ProteinConversion -> m QuantifiedNodeData
entireTree = fromNutTreeWithMass_ standardMass . nutHierarchy

fullToDisplayTree :: QuantifiedNodeData -> DisplayNode Mass
fullToDisplayTree = uncurry (toNode standardMass)
  where
    toNode v ks = uncurry (DisplayNode v) . unpackNodes v ks

    unpackNodes mass ks u = case u of
      Right fn -> (toMap (Quantified fn : ks), mempty)
      Left uts -> case N.nonEmpty ks of
        Nothing -> (mempty, withNonEmpty (`M.singleton` mass) mempty uts)
        Just ks' ->
          (toMap $ toList ks', M.singleton uts (mass - sumTrees ks'))

    partialToDisplayTree UnquantifiedNode {pnNut, pnKnown} =
      ( pnNut
      , DisplayNode (sumTrees pnKnown) (toMap $ toList pnKnown) mempty
      )

    fullToDisplayTree_ QuantifiedNode {fnNut, fnValue, fnKnown, fnUnknown} =
      (fnNut, toNode fnValue fnKnown fnUnknown)

    toMap = M.fromList . fmap go

    go (Quantified n) = fullToDisplayTree_ n
    go (Unquantified n) = partialToDisplayTree n

displayTree :: NutrientState m => ProteinConversion -> m (DisplayNode Mass)
displayTree = fmap fullToDisplayTree . entireTree

findMass :: NutrientState m => NID -> m (Maybe Mass)
findMass i =
  state (first (fmap vnAmount) . M.updateLookupWithKey (\_ _ -> Nothing) i)

findMeasured :: NutrientState m => MeasuredNutrient -> m (Maybe Mass)
findMeasured n = case n of
  Direct m -> findMass $ mnId m
  Alternate AltNutrient {anChoices} -> foldM go Nothing anChoices
  where
    go Nothing (i, s) = liftA2 (*) (Mass <$> s) <$> findMass i
    go m _ = pure m

fromNutTreeWithMass_ :: NutrientState m => Mass -> NutTree -> m QuantifiedNodeData
fromNutTreeWithMass_ mass NutTree {ntFractions, ntUnmeasuredHeader, ntUnmeasuredTree} = do
  -- possible results from this operation:
  -- 1) all known -> we know the mass of the unmeasured tree
  -- 2) at least one unknown -> we don't know the mass of the unknown tree
  res <- readBranches ntFractions
  case res of
    -- at least one unknown
    Left (ks, ms) -> do
      (umk, umus) <- case ntUnmeasuredTree of
        -- no unmeasured tree -> the unmeasured header just gets put with the
        -- other unknowns
        Nothing -> return (Nothing, [])
        -- unmeasured tree present. Where will at least be one unknown by
        -- by definition, since this tree has an unmeasured category. There
        -- may or may not be any knowns. Any knowns (if present) get put under
        -- the unmeasured header in their own mass. The unknowns get aggregated.
        -- Note that the unmeasured header will appear twice if the unmeasured
        -- tree has a known component, since the header will only be partly
        -- explained.
        Just ut ->
          bimap
            -- NOTE by definition there cannot be unknowns in this tree;
            -- since we don't know the total mass this tree should represent
            -- all the unknowns must "propagate up a level"
            (fmap (Unquantified . UnquantifiedNode umh) . N.nonEmpty)
            toList
            <$> fromNutTreeWithoutMass ut
      return (maybe ks (: ks) umk, Left $ UnknownTree umn umus : toList ms)
    -- all known
    Right ks -> do
      let diffMass = mass - sumTrees ks
      -- make a leaf or a tree depending on if we have an unmeasured tree
      -- to put under the header
      um <-
        maybe
          (return $ QuantifiedNode diffMass umh [] (Left []))
          (fromNutTreeWithMass diffMass umh)
          ntUnmeasuredTree
      return (toList ks, Right um)
  where
    umh = summedToDisplay ntUnmeasuredHeader
    umn = snName ntUnmeasuredHeader

fromNutTreeWithMass
  :: NutrientState m
  => Mass
  -> DisplayNutrient
  -> NutTree
  -> m QuantifiedNode
fromNutTreeWithMass mass dn nt = do
  (ks, us) <- fromNutTreeWithMass_ mass nt
  return $ QuantifiedNode mass dn ks us

fromNutTreeWithoutMass
  :: NutrientState m
  => NutTree
  -> m ([ParsedTreeNode], NonEmpty UnknownTree)
fromNutTreeWithoutMass NutTree {ntFractions, ntUnmeasuredHeader, ntUnmeasuredTree} = do
  (umk, umu) <- case ntUnmeasuredTree of
    Nothing -> return (Nothing, [])
    Just ut -> bimap (fmap go . N.nonEmpty) toList <$> fromNutTreeWithoutMass ut

  (ks, us) <- either (second toList) ((,[]) . toList) <$> readBranches ntFractions

  return (maybe ks (: ks) umk, UnknownTree umn umu :| us)
  where
    umh = summedToDisplay ntUnmeasuredHeader
    umn = snName ntUnmeasuredHeader
    go = Unquantified . UnquantifiedNode umh

readBranches
  :: NutrientState m
  => Branches
  -> m
      ( Either
          ([ParsedTreeNode], NonEmpty UnknownTree)
          (NonEmpty ParsedTreeNode)
      )
readBranches bs = do
  (r :| rs) <- mapM fromAgg bs
  return $ foldr combineRes r rs
  where
    -- TODO better way to do this? this is almost like the Either instance for
    -- Traversible (ie mapM) except the Lefts are also being combined
    combineRes (Right ks0) (Right ks1) = Right $ ks0 <> ks1
    combineRes (Right ks0) (Left (ks1, us)) = Left (toList ks0 ++ ks1, us)
    combineRes (Left (ks0, us)) (Right ks1) = Left (ks0 ++ toList ks1, us)
    combineRes (Left (ks0, us0)) (Left (ks1, us1)) = Left (ks0 ++ ks1, us0 <> us1)

    fromAgg (NutrientSingle x) = fromHeader x
    fromAgg (NutrientMany (x :| xs)) = do
      init <- fromHeader x
      foldM go init xs
      where
        go (Left ([], _)) next = fromHeader next
        go acc next = do
          -- if we have found a valid nutrient that takes priority over others
          -- remove the others from the nutrient map so they don't end up in
          -- the "unused" debug output
          _ <- case next of
            (MeasuredHeader h _) -> findMeasured h
            (Leaf h) -> findMeasured h
            _ -> return Nothing
          return acc

    fromHeader (MeasuredHeader h nt) = do
      let dh = measToDisplay h
      mass <- findMeasured h
      case mass of
        Just m -> Right . pure . Quantified <$> fromNutTreeWithMass m dh nt
        Nothing ->
          Left . bimap (fromKnowns dh) (fromUnknowns dh)
            <$> fromNutTreeWithoutMass nt
    fromHeader (UnmeasuredHeader h bs') = do
      let dh = summedToDisplay h
      bimap (bimap (fromKnowns dh) (fromUnknowns dh)) (toKnownTree dh) <$> readBranches bs'
    fromHeader (Leaf h) = do
      let dh = measToDisplay h
      mass <- findMeasured h
      return $ case mass of
        Just m -> Right $ pure $ Quantified $ QuantifiedNode m dh [] (Left [])
        Nothing -> Left ([], pure $ UnknownTree (dnName dh) [])

    fromKnowns _ [] = []
    fromKnowns dh (k : ks) = [Unquantified $ UnquantifiedNode dh (k :| ks)]

    fromUnknowns dh = pure . UnknownTree (dnName dh) . toList

    toKnownTree dh = pure . Unquantified . UnquantifiedNode dh

sumTrees :: NonEmpty ParsedTreeNode -> Mass
sumTrees (f :| fs) = foldr (\n -> (+ go n)) (go f) fs
  where
    go (Quantified n) = fnValue n
    go (Unquantified n) = sumTrees $ pnKnown n

summedToDisplay :: SummedNutrient -> DisplayNutrient
summedToDisplay (SummedNutrient x y) = DisplayNutrient x y

measToDisplay :: MeasuredNutrient -> DisplayNutrient
measToDisplay (Direct (DirectNutrient _ n p)) = DisplayNutrient n p
measToDisplay (Alternate (AltNutrient n p _)) = DisplayNutrient n p

--------------------------------------------------------------------------------
-- misc types

type ExportIngredientMetadata = IngredientMetadata [Modification] DaySpan

type SummaryIngredientMetadata = IngredientMetadata () Day

data IngredientMetadata ms d = IngredientMetadata
  { imMeal :: MealGroup
  , imMass :: Mass
  , imMods :: ms
  , imDaySpan :: d
  }
  deriving (Show)

data ValidSchedule = ValidSchedule
  { vsIngs :: NonEmpty Ingredient
  , vsMeal :: MealGroup
  , vsCron :: Cron
  , vsScale :: Scientific
  }

type ValidCustomMap = Map Text MappedFoodItem

type NutrientState = MonadState NutrientMap

data UnusedNutrient = UnusedNutrient MealGroup NID ValidNutrient
  deriving (Eq)

type DisplayTrees = NonEmpty (DisplayTree GroupByAll)

type CustomID = Text

type Source = Either FID CustomID

type QuantifiedNodeData = ([ParsedTreeNode], Either [UnknownTree] QuantifiedNode)
