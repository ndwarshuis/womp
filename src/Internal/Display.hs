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
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N

toSumTree :: DisplayTree g -> (g, DisplayTreeSum)
toSumTree (DisplayTree_ ms e g) = (g, bimap Sum Sum $ DisplayTree_ ms e ())

fromSumTree :: g -> DisplayTreeSum -> DisplayTree g
fromSumTree g (DisplayTree_ ms e _) =
  bimap getSum getSum $ DisplayTree_ ms e g

groupTrees
  :: Eq g1
  => (g0 -> g1)
  -> NonEmpty (DisplayTree g0)
  -> NonEmpty (DisplayTree g1)
groupTrees f =
  fmap go
    . N.groupWith1 fst
    . fmap (first f . toSumTree)
  where
    go xs@((g, _) :| _) = fromSumTree g $ sconcat $ fmap snd xs

treeToJSON
  :: AllTreeDisplayOptions
  -> GroupOptions
  -> NonEmpty (DisplayTree GroupByAll)
  -> NonEmpty Value
treeToJSON dos gos = chooseGrouping gos (fmap (treeToJSON_ dos))

treeToCSV
  :: AllTabularDisplayOptions
  -> C.EncodeOptions
  -> GroupOptions
  -> NonEmpty (DisplayTree GroupByAll)
  -> BL.ByteString
treeToCSV tos eos gos = chooseGrouping gos go
  where
    go
      :: ( Ord d
         , Ord m
         , Ord i
         , C.ToNamedRecord (DisplayRow (GroupVars d m i))
         , C.DefaultOrdered (DisplayRow (GroupVars d m i))
         )
      => NonEmpty (DisplayTree (GroupVars d m i))
      -> BL.ByteString
    go =
      C.encodeDefaultOrderedByNameWith eos
        . N.toList
        . N.sortBy (compareRow (atabSort tos))
        . sconcat
        . fmap (treeToRows tos)

-- TODO not sure how to get the instance constraints out of the rankN function
-- (they don't seem necessary)
chooseGrouping
  :: GroupOptions
  -> ( forall d m i
        . ( Ord d
          , Ord m
          , Ord i
          , ToJSON (GroupVars d m i)
          , C.DefaultOrdered (DisplayRow (GroupVars d m i))
          , C.ToNamedRecord (DisplayRow (GroupVars d m i))
          )
       => NonEmpty (DisplayTree (GroupVars d m i))
       -> a
     )
  -> NonEmpty (DisplayTree GroupByAll)
  -> a
chooseGrouping (GroupOptions d m i) f ts = case (d, m, i) of
  (True, True, True) -> f $ groupTrees id ts
  (True, True, False) -> f $ groupTrees (\g -> g {gvIngredient = ()}) ts
  (True, False, True) -> f $ groupTrees (\g -> g {gvMeal = ()}) ts
  (True, False, False) -> f $ groupTrees (\g -> g {gvMeal = (), gvIngredient = ()}) ts
  (False, True, True) -> f $ groupTrees (\g -> g {gvDaySpan = ()}) ts
  (False, True, False) -> f $ groupTrees (\g -> g {gvDaySpan = (), gvIngredient = ()}) ts
  (False, False, True) -> f $ groupTrees (\g -> g {gvDaySpan = (), gvMeal = ()}) ts
  (False, False, False) -> f $ groupTrees (const (GroupVars () () ())) ts

treeToJSON_ :: ToJSON g => AllTreeDisplayOptions -> DisplayTree g -> Value
treeToJSON_ o (DisplayTree_ ms e g) =
  object
    [ "group" .= toJSON g
    , "energy"
        .= object
          [ "value" .= roundDigits (atdoRoundDigits o) e
          , "unit" .= Unit Kilo Calorie
          ]
    , "mass" .= nodeToJSON o "Total" Unity ms
    ]

nodeToJSON
  :: AllTreeDisplayOptions
  -> Text
  -> Prefix
  -> DisplayNode Mass
  -> Value
nodeToJSON o@(AllTreeDisplayOptions u e uy r) n p (DisplayNode v ks us) =
  object $
    [ encodeValue v'
    , "name" .= n
    , encodeUnit p'
    ]
      ++ maybe [] ((: []) . ("known" .=)) (N.nonEmpty $ mapK ks)
      ++ ["unknown" .= mapU us | doExpandedUnits u]
  where
    (p', v') = unityMaybe uy p $ unMass v

    mapK = fmap (\(DisplayNutrient n' p'', v'') -> nodeToJSON o n' p'' v'') . M.toList

    mapU = fmap (uncurry goUnk) . M.toList

    goUnk uts v'' =
      let (p'', v''') =
            unityMaybe uy (autoPrefix $ unMass v'') $ unMass v''
       in object
            [ encodeValue v'''
            , encodeUnit p''
            , "trees" .= uts
            ]

    encodeValue = ("value" .=) . roundDigits r
    encodeUnit p'' =
      let u' = Unit p'' Gram
       in if e then "unit" .= u' else "unit" .= tunit u'

-- | Convert a value and its prefix depending on if we want all values to be
-- displayed in their native units or all in unity (ie "grams" with no prefix).
-- Assume the incoming value is in grams
unityMaybe :: Bool -> Prefix -> Scientific -> (Prefix, Scientific)
unityMaybe True _ v = (Unity, v)
unityMaybe False p v = (p, raisePower (-prefixValue p) v)

toUnity :: Prefix -> Scientific -> Scientific
toUnity p = raisePower (prefixValue p)

treeToRows :: AllTabularDisplayOptions -> DisplayTree g -> NonEmpty (DisplayRow g)
treeToRows (AllTabularDisplayOptions su uu r _) (DisplayTree_ (DisplayNode v ks us) e g) =
  energy :| (totalMass : (goK massName ks ++ [goU massName us | su]))
  where
    row = DisplayRow g

    dpyRow n pnt v' u =
      let (p'', v'') = unityMaybe uu (unitBase u) v'
       in row n pnt (roundDigits r v'') (u {unitBase = p''})

    -- multiply by 1000 here since calories are actually given in kcal
    energy = dpyRow "Energy" Nothing (unEnergy e * 1000) (Unit Kilo Calorie)

    totalMass = dpyRow "Total Mass" Nothing (unMass v) (Unit Unity Gram)

    massRow n pnt v' p = dpyRow n (Just pnt) v' (Unit p Gram)

    massName = "Total Mass"

    unknownName = "Unknown"

    goK pnt = concatMap (uncurry (goK_ pnt)) . M.assocs

    goK_ pnt (DisplayNutrient n p) (DisplayNode v' ks' us') =
      massRow n pnt (unMass v') p : (goK n ks' ++ [goU n us' | su])

    goU pnt us' =
      let s = unMass $ sum $ M.elems us'
          p = autoPrefix s
       in massRow unknownName pnt s p

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
      go (unitName . drUnit) x y
        <> go getValue x y
        <> go (unitBase . drUnit) x y
    getValue x = toUnity (unitBase $ drUnit x) (drValue x)

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