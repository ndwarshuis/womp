module Internal.Display
  ( treeToJSON
  , treeToCSV
  , dumpNutrientTree
  )
where

import Data.Aeson
import qualified Data.Csv as C
import Data.Monoid
import Data.Scientific
import Data.Semigroup (sconcat)
import Internal.Nutrients
import Internal.Types.CLI
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.List as L
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import Text.Regex.TDFA

--------------------------------------------------------------------------------
-- tree (json/yaml)

treeToJSON
  :: AllTreeDisplayOptions
  -> GroupOptions
  -> [DisplayTree GroupByAll]
  -> [Value]
treeToJSON dos gos = groupAndFilter gos (fmap (treeToJSON_ dos)) (atdoFilter dos)

treeToJSON_ :: ToJSON g => AllTreeDisplayOptions -> DisplayTree g -> Value
treeToJSON_ o (DisplayTree_ ms e g) =
  object
    [ "group" .= toJSON g
    , "energy"
        .= object
          [ "value" .= roundDigits (atdoRoundDigits o) e
          , "unit" .= kcal
          ]
    , "mass" .= (uncurry (nodeToJSON o) <$> M.toList ms)
    ]

nodeToJSON
  :: AllTreeDisplayOptions
  -> DisplayNutrient
  -> DisplayNode Mass
  -> Value
nodeToJSON o@(AllTreeDisplayOptions u e uy r _) (DisplayNutrient n p) (DisplayNode v ks us) =
  object $
    [ encodeValue v'
    , "name" .= n
    , encodeUnit p'
    ]
      ++ maybe [] ((: []) . ("known" .=)) (N.nonEmpty $ mapK ks)
      ++ ["unknown" .= mapU us | doExpandedUnits u]
  where
    (p', v') = convertWithPrefix uy p r $ unMass v

    mapK = fmap (uncurry (nodeToJSON o)) . M.toList

    mapU = fmap (uncurry goUnk) . M.toList

    goUnk uts v'' =
      let (p'', v''') =
            convertWithPrefix uy (autoPrefix $ unMass v'') r $ unMass v''
       in object
            [ encodeValue v'''
            , encodeUnit p''
            , "trees" .= uts
            ]

    encodeValue = ("value" .=)
    encodeUnit p'' =
      let u' = Unit p'' Gram
       in if e then "unit" .= u' else "unit" .= tunit u'

--------------------------------------------------------------------------------
-- table (tsv/csv/psv/whatever-sv)

treeToCSV
  :: AllTabularDisplayOptions
  -> C.EncodeOptions
  -> GroupOptions
  -> [DisplayTree GroupByAll]
  -> BL.ByteString
treeToCSV tos eos gos = groupAndFilter gos go (atabFilter tos)
  where
    go :: ThisGroup d m i => [DisplayTree (GroupVars d m i)] -> BL.ByteString
    go =
      C.encodeDefaultOrderedByNameWith eos
        . L.sortBy (compareRow (atabSort tos))
        . mconcat
        . fmap (treeToRows tos)

treeToRows :: AllTabularDisplayOptions -> DisplayTree g -> [DisplayRow g]
treeToRows (AllTabularDisplayOptions su uu r _ _) (DisplayTree_ ms e g) =
  -- TODO this "Nothing" won't be valid in general after filtering (unless we
  -- don't want parental information to be retained for the top of any selected
  -- tree)
  energy : goK Nothing ms
  where
    row = DisplayRow g

    energy = row "Energy" Nothing (unEnergy $ roundDigits r e) kcal

    massRow n pnt v p =
      let (p', v') = convertWithPrefix uu p r $ unMass v
       in row n pnt v' (Unit p' Gram)

    goK pnt = concatMap (uncurry (goK_ pnt)) . M.assocs

    goK_ pnt (DisplayNutrient n p) (DisplayNode v ks us) =
      massRow n pnt v p : (goK (Just n) ks ++ [goU n us | su])

    goU pnt us' =
      let s = sum $ M.elems us'
       in massRow "Unknown" (Just pnt) s (autoPrefix s)

--------------------------------------------------------------------------------
-- grouping (which includes filtering)

-- | Group and filter a list of trees. First, filter by the group variables we
-- with to select (ie meal and ingredient). Then group by whatever variables we
-- desired (meal, ingredient, date) which entails grouping the list by the
-- appropriate variable then (<>)-ing them together. Finally, filter the
-- contents of each tree as desired (value and nutrient). This last step
-- actually needs to come last and cannot happen with the other grouping step
-- since the value comparison must be done on the post-grouped data (or at least
-- that's what makes most sense)
groupAndFilter
  :: forall a
   . GroupOptions
  -> (forall d m i. ThisGroup d m i => [DisplayTree (GroupVars d m i)] -> a)
  -> [FilterKey]
  -> [DisplayTree GroupByAll]
  -> a
groupAndFilter (GroupOptions d m i) f fks ts = case (d, m, i) of
  -- TODO not sure how to get the instance constraints out of the rankN function
  -- (they don't seem necessary)
  (True, True, True) -> f' $ groupTrees id ts'
  (True, True, False) -> f' $ groupTrees (\g -> g {gvIngredient = ()}) ts'
  (True, False, True) -> f' $ groupTrees (\g -> g {gvMeal = ()}) ts'
  (True, False, False) -> f' $ groupTrees (\g -> g {gvMeal = (), gvIngredient = ()}) ts'
  (False, True, True) -> f' $ groupTrees (\g -> g {gvDaySpan = ()}) ts'
  (False, True, False) -> f' $ groupTrees (\g -> g {gvDaySpan = (), gvIngredient = ()}) ts'
  (False, False, True) -> f' $ groupTrees (\g -> g {gvDaySpan = (), gvMeal = ()}) ts'
  (False, False, False) -> f' $ groupTrees (const (GroupVars () () ())) ts'
  where
    (gfs, tfs) = partitionFilterKeys fks
    f' :: forall d m i. ThisGroup d m i => [DisplayTree (GroupVars d m i)] -> a
    f' = f . fmap (filterTreeKeys tfs)
    ts' = mapMaybe (filterGroupKeys gfs) ts

toSumTree :: DisplayTree g -> (g, DisplayTreeSum)
toSumTree (DisplayTree_ ms e g) = (g, bimap Sum Sum $ DisplayTree_ ms e ())

fromSumTree :: g -> DisplayTreeSum -> DisplayTree g
fromSumTree g (DisplayTree_ ms e _) =
  bimap getSum getSum $ DisplayTree_ ms e g

groupTrees
  :: Eq g1
  => (g0 -> g1)
  -> [DisplayTree g0]
  -> [DisplayTree g1]
groupTrees f =
  fmap go
    . N.groupWith fst
    . fmap (first f . toSumTree)
  where
    go xs@((g, _) :| _) = fromSumTree g $ sconcat $ fmap snd xs

--------------------------------------------------------------------------------
-- sorting

compareRow
  :: (Ord d, Ord m, Ord i)
  => [SortKey]
  -> DisplayRow (GroupVars d m i)
  -> DisplayRow (GroupVars d m i)
  -> Ordering
compareRow ks a b = maybe EQ (sconcat . fmap (compareRowKey a b)) $ N.nonEmpty ks

compareRowKey
  :: (Ord d, Ord m, Ord i)
  => DisplayRow (GroupVars d m i)
  -> DisplayRow (GroupVars d m i)
  -> SortKey
  -> Ordering
compareRowKey a b SortKey {skField, skAsc} = case (f a b, skAsc) of
  (LT, True) -> GT
  (GT, True) -> LT
  (EQ, _) -> EQ
  (x, False) -> x
  where
    f = case skField of
      SortDate -> go (gvDaySpan . drGroup)
      SortMeal -> go (gvMeal . drGroup)
      SortIngredient -> go (gvIngredient . drGroup)
      SortNutrient -> go drNutrient
      SortParent -> go drParentNutrient
      SortValue -> compareValue
    go g x y = compare (g x) (g y)
    compareValue x y =
      -- compare units first so that calories and grams will sort separately,
      -- and sort the prefix last such that "small looking" things are below
      -- "large looking" things
      go (unitMeasurement . drUnit) x y
        <> go getValue x y
        <> go (unitPrefix . drUnit) x y
    getValue x = toUnity (unitPrefix $ drUnit x) (drValue x)

--------------------------------------------------------------------------------
-- filtering

partitionFilterKeys :: [FilterKey] -> ([GroupFilterKey], [TreeFilterKey])
partitionFilterKeys = partitionEithers . fmap go
  where
    go (GroupFilter f) = Left f
    go (TreeFilter f) = Right f

filterGroupKeys :: [GroupFilterKey] -> DisplayTree GroupByAll -> Maybe (DisplayTree GroupByAll)
filterGroupKeys = flip $ foldM (flip filterGroup)

filterTreeKeys :: [TreeFilterKey] -> DisplayTree g -> DisplayTree g
filterTreeKeys = flip $ foldr filterTree

filterGroup :: GroupFilterKey -> DisplayTree GroupByAll -> Maybe (DisplayTree GroupByAll)
filterGroup (GroupFilterKey keep re kt) t
  | keep == f re t = Just t
  | otherwise = Nothing
  where
    f = case kt of
      FilterMeal -> matchesMeal
      FilterIngredient -> matchesIngredient

filterTree :: TreeFilterKey -> DisplayTree g -> DisplayTree g
filterTree (TreeFilterKey keep k) t@(DisplayTree_ ms _ _) =
  t {dtMap = f ms}
  where
    f =
      if keep
        then selectTreesWith matchFun
        else removeTreesWith (fmap not . matchFun)
    matchFun = case k of
      FilterNutrient re -> (\n _ -> n =~ re)
      FilterValue x op -> (\_ v -> opFun op v x)

opFun :: Ord a => Operator -> a -> a -> Bool
opFun EQ_ = (==)
opFun LT_ = (<)
opFun GT_ = (>)
opFun LTE_ = (<=)
opFun GTE_ = (>=)

matchesMeal :: Text -> DisplayTree GroupByAll -> Bool
matchesMeal f (DisplayTree_ _ _ GroupVars {gvMeal}) = unMealGroup gvMeal =~ f

matchesIngredient :: Text -> DisplayTree GroupByAll -> Bool
matchesIngredient f (DisplayTree_ _ _ GroupVars {gvIngredient}) =
  unIngredientGroup gvIngredient =~ f

selectTreesWith :: (Text -> a -> Bool) -> DisplayMap a -> DisplayMap a
selectTreesWith f = M.fromList . concatMap (uncurry go) . M.toList
  where
    go dn@(DisplayNutrient n _) d@(DisplayNode v ks _)
      | f n v = [(dn, d)]
      | otherwise = M.toList $ selectTreesWith f ks

removeTreesWith :: (Text -> a -> Bool) -> DisplayMap a -> DisplayMap a
removeTreesWith f = M.mapMaybeWithKey go
  where
    go (DisplayNutrient n _) d@(DisplayNode v ks _)
      | f n v = Just $ d {dnKnown = removeTreesWith f ks}
      | otherwise = Nothing

--------------------------------------------------------------------------------
-- misc

kcal :: Unit
kcal = Unit Kilo Calorie

-- | Convert and round a number given its default prefix or a prefix we choose
-- if supplied
convertWithPrefix :: Maybe Prefix -> Prefix -> Int -> Scientific -> (Prefix, Scientific)
convertWithPrefix p dp r v = (p', roundDigits r $ raisePower (-prefixValue p') v)
  where
    p' = fromMaybe dp p

dumpNutrientTree :: [NutTreeRow]
dumpNutrientTree = goTree Nothing $ nutHierarchy 0
  where
    goTree parent (NutTree bs umh umt) =
      concatMap (goBranch parent) bs
        ++ [goSummed parent umh]
        ++ maybe [] (goTree parent) umt

    goBranch parent (NutrientSingle n) = goNode parent n
    goBranch parent (NutrientMany ns) = concatMap (goNode parent) ns

    goNode parent (MeasuredHeader h t) =
      goMeasured parent h ++ goTree (Just $ measuredName h) t
    goNode parent (UnmeasuredHeader h bs) =
      goSummed parent h : concatMap (goBranch (Just $ snName h)) bs
    goNode parent (Leaf m) = goMeasured parent m

    measuredName (Direct DirectNutrient {mnName}) = mnName
    measuredName (Alternate AltNutrient {anName}) = anName

    goMeasured parent m = case m of
      (Direct DirectNutrient {mnName, mnId}) ->
        [NutTreeRow mnName parent $ Just mnId]
      (Alternate AltNutrient {anName, anChoices}) ->
        NutTreeRow anName parent . Just . fst <$> N.toList anChoices

    goSummed parent SummedNutrient {snName} = NutTreeRow snName parent Nothing

type ThisGroup d m i =
  ( Ord d
  , Ord m
  , Ord i
  , ToJSON (GroupVars d m i)
  , C.DefaultOrdered (DisplayRow (GroupVars d m i))
  , C.ToNamedRecord (DisplayRow (GroupVars d m i))
  )
