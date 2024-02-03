module Internal.NutrientTree
  ( nutHierarchy
  , displayTree
  , lookupTree
  , summedToDisplay
  , measToDisplay
  , treeToJSON
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
import RIO.State

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

type FullNodeData = ([ParsedTreeNode Mass], Either [UnknownTree] (QuantifiedNode Mass))

fromNutTreeWithMass_ :: NutrientState m => Mass -> NutTree -> m FullNodeData
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
  -> m (QuantifiedNode Mass)
fromNutTreeWithMass mass dn nt = do
  (ks, us) <- fromNutTreeWithMass_ mass nt
  return $ QuantifiedNode mass dn ks us

fromNutTreeWithoutMass
  :: NutrientState m
  => NutTree
  -> m ([ParsedTreeNode Mass], NonEmpty UnknownTree)
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
          ( [ParsedTreeNode Mass]
          , NonEmpty UnknownTree
          )
          (NonEmpty (ParsedTreeNode Mass))
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

sumTrees :: Num a => NonEmpty (ParsedTreeNode a) -> a
sumTrees (f :| fs) = foldr (\n -> (+ go n)) (go f) fs
  where
    go (Quantified n) = fnValue n
    go (Unquantified n) = sumTrees $ pnKnown n

summedToDisplay :: SummedNutrient -> DisplayNutrient
summedToDisplay (SummedNutrient x y) = DisplayNutrient x y

measToDisplay :: MeasuredNutrient -> DisplayNutrient
measToDisplay (Direct (DirectNutrient _ n p)) = DisplayNutrient n p
measToDisplay (Alternate (AltNutrient n p _)) = DisplayNutrient n p

entireTree
  :: NutrientState m
  => ProteinConversion
  -> m ([ParsedTreeNode Mass], Either [UnknownTree] (QuantifiedNode Mass))
entireTree = fromNutTreeWithMass_ standardMass . nutHierarchy

fullToDisplayTree :: FullNodeData -> DisplayNode Mass
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

-- TODO not sure how efficient this is; probably doesn't matter than much ;)
lookupTree :: DisplayNutrient -> DisplayNode a -> Maybe a
lookupTree k (DisplayNode _ ks _) =
  msum ((dnValue <$> M.lookup k ks) : (lookupTree k <$> M.elems ks))

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

type NutrientState = MonadState NutrientMap
