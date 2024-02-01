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
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.ByteString.Lazy as BL
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import RIO.State
import qualified RIO.Text as T

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

allPhytosterols :: NonEmpty MeasuredNutrient
allPhytosterols =
  stigmastadiene
    :| [ stigmastadiene
       , stigmasterol
       , campesterol
       , brassicasterol
       , betaSitosterol
       , campestanol
       , betaSitostanol
       , delta_5_avenasterol
       , delta_7_stigmastenol
       , otherPhytosterols
       , ergosterol
       , ergosta_7_enol
       , ergosta_7_22_dienol
       , ergosta_5_7_dienol
       ]

allAminoAcids :: NonEmpty MeasuredNutrient
allAminoAcids =
  tryptophan
    :| [ threonine
       , isoleucine
       , leucine
       , lysine
       , methionine
       , cystine
       , phenylalanine
       , tyrosine
       , valine
       , arginine
       , histidine
       , alanine
       , asparticAcid
       , glutamicAcid
       , glycine
       , proline
       , serine
       , hydroxyproline
       , asparagine
       , cysteine
       , glutamine
       ]

allMinerals :: NonEmpty MeasuredNutrient
allMinerals =
  boron
    :| [ sodium
       , magnesium
       , phosphorus
       , sulfur
       , potassium
       , calcium
       , chromium
       , manganese
       , iron
       , cobalt
       , nickel
       , copper
       , zinc
       , selenium
       , molybdenum
       , iodine
       ]

allTFAs :: NonEmpty MeasuredNutrient
allTFAs =
  tfa_14_1
    :| [tfa_16_1, tfa_17_1, tfa_18_1, tfa_18_2, tfa_18_3, tfa_20_1, tfa_22_1]

allSugars :: NonEmpty MeasuredNutrient
allSugars =
  sucrose
    :| [ glucose
       , fructose
       , lactose
       , maltose
       , galactose
       , raffinose
       , stachyose
       , verbascose
       ]

allSFAs :: NonEmpty MeasuredNutrient
allSFAs =
  sfa_4_0
    :| [ sfa_5_0
       , sfa_6_0
       , sfa_7_0
       , sfa_8_0
       , sfa_9_0
       , sfa_10_0
       , sfa_11_0
       , sfa_12_0
       , sfa_14_0
       , sfa_15_0
       , sfa_16_0
       , sfa_17_0
       , sfa_18_0
       , sfa_20_0
       , sfa_21_0
       , sfa_22_0
       , sfa_23_0
       , sfa_24_0
       ]

allIsoflavones :: NonEmpty MeasuredNutrient
allIsoflavones = daidzein :| [daidzin, genistein, genistin, glycitin]

allVitaminE :: NonEmpty MeasuredNutrient
allVitaminE =
  tocopherolAlpha
    :| [ tocopherolBeta
       , tocopherolGamma
       , tocopherolDelta
       , tocotrienolAlpha
       , tocotrienolBeta
       , tocotrienolGamma
       , tocotrienolDelta
       ]

allVitaminA :: NonEmpty MeasuredNutrient
allVitaminA =
  retinol
    :| [ alphaCarotene
       , betaCarotene
       , cisBetaCarotene
       , transBetaCarotene
       , gammaCarotene
       , alphaCryptoxanthin
       , betaCryptoxanthin
       ]

allCholine :: NonEmpty MeasuredNutrient
allCholine =
  freeCholine
    :| [phosphoCholine, phosphotidylCholine, glycerophosphoCholine, sphingomyelinCholine]

nutHierarchy :: ProteinConversion -> NutTree
nutHierarchy n2Factor =
  NutTree
    { ntFractions =
        leaf water
          :| [ measuredLeaves (protein n2Factor) otherProteinMass allAminoAcids
             , measuredLeaves ash otherInorganics allMinerals
             , measured lipid $
                nutTree
                  otherLipids
                  ( leaf cholesterol
                      :| [ measuredLeaves tfas otherTFAs allTFAs
                         , measuredLeaves sfas otherSFAs allSFAs
                         , measured pufas pufas_
                         , measured mufas mufas_
                         , unmeasuredLeaves phytosterols allPhytosterols
                         ]
                  )
             ]
    , ntUnmeasuredHeader = carbDiff
    , ntUnmeasuredTree = Just carbs
    }
  where
    leaf = AggIdentity . Leaf
    group n p = AggIdentity . UnmeasuredHeader (SummedNutrient n p)
    measured h = AggIdentity . MeasuredHeader h
    unmeasured h = AggIdentity . UnmeasuredHeader h
    nutTree u xs =
      NutTree
        { ntFractions = xs
        , ntUnmeasuredHeader = u
        , ntUnmeasuredTree = Nothing
        }
    measuredLeaves h u xs = measured h $ nutTree u (leaf <$> xs)
    unmeasuredLeaves h xs = unmeasured h (leaf <$> xs)
    groupLeaves h u xs = group h u (leaf <$> xs)
    unclassified x u = SummedNutrient (T.append x " (unclassified)") u

    carbs =
      nutTree otherCarbs $
        leaf starch
          :| [ leaf betaGlucan
             , unmeasured totalSugars $ leaf <$> allSugars
             , fiber
             , vitamins
             , organics
             ]

    fiber =
      Priority
        ( MeasuredHeader
            fiberBySolubility
            ( nutTree otherFiberBySolubility $
                fmap leaf (solubleFiber :| [insolubleFiber])
            )
            :| [ MeasuredHeader
                  fiberByWeight
                  ( nutTree otherFiberByWeight $
                      fmap leaf (highMWFiber :| [lowMWFiber])
                  )
               ]
        )

    organics =
      group "Organics" Milli $
        lycopene_
          :| [ luteins_
             , unmeasuredLeaves isoflavones allIsoflavones
             , measuredLeaves choline otherCholine allCholine
             , leaf betaine
             , leaf citricAcid
             , leaf malicAcid
             , leaf oxalicAcid
             , leaf pyruvicAcid
             , leaf quinicAcid
             , leaf taurine
             , leaf ergothioneine
             , leaf phytoene
             , leaf phytofluene
             ]

    lycopene_ =
      measuredLeaves lycopene (unclassified "Lycopenes" Milli) $
        transLycopene :| [cisLycopene]

    luteins_ =
      measuredLeaves luteins (unclassified "Luteins" Milli) $
        transLutein :| [cisLutein, zeaxanthin]

    vitamins =
      group "Vitamins" Milli $
        groupLeaves "Vitamin A" Micro allVitaminA
          :| [ group "Vitamin B" Milli $
                leaf vitaminB1
                  :| [ leaf vitaminB2
                     , leaf vitaminB3
                     , leaf vitaminB5
                     , leaf vitaminB6
                     , leaf vitaminB7
                     , measuredLeaves vitaminB9 otherFolate $ folinicAcid :| [levomefolicAcid]
                     , leaf vitaminB12
                     ]
             , leaf vitaminC
             , unmeasured vitaminD $
                leaf calcifediol
                  :| [ leaf vitaminD4
                     , measuredLeaves vitaminD2andD3 otherVitaminD $
                        vitaminD2 :| [vitaminD3]
                     ]
             , groupLeaves "Vitamin E" Milli allVitaminE
             , group "Vitamin K" Micro $
                fmap leaf $
                  vitaminK1 :| [vitaminK2, dihydrophylloquinone]
             ]

    mufas_ =
      nutTree otherMUFAs $
        leaf mufa_12_1
          :| [ leaf mufa_14_1
             , leaf mufa_15_1
             , leaf mufa_16_1
             , leaf mufa_17_1
             , leaf mufa_18_1
             , leaf mufa_20_1
             , measuredLeaves mufa_22_1 mufa_22_1_other $
                mufa_22_1_n11 :| [mufa_22_1_n9]
             , leaf mufa_24_1
             ]

    pufas_ =
      nutTree otherPUFAs $
        measuredLeaves
          pufa_18_2
          pufa_18_2_other
          (pufa_18_2_CLA :| [pufa_18_2_n6_cc])
          :| [ measuredLeaves
                pufa_18_3
                pufa_18_3_other
                (pufa_18_3_n3_ccc :| [pufa_18_3_n6_ccc, pufa_18_3i])
             , leaf pufa_18_4
             , unmeasuredLeaves pufa_20_2 (pufa_20_2_n6_cc :| [])
             , measuredLeaves
                pufa_20_3
                pufa_20_3_other
                (pufa_20_3_n3 :| [pufa_20_3_n6, pufa_20_3_n9])
             , leaf pufa_20_4
             , unmeasuredLeaves pufa_20_5 (pufa_20_5_n3 :| [])
             , leaf pufa_22_2
             , leaf pufa_22_3
             , leaf pufa_22_4
             , unmeasuredLeaves pufa_22_5 (pufa_22_5_n3 :| [])
             , unmeasuredLeaves pufa_22_6 (pufa_22_6_n3 :| [])
             ]

type FullNodeData = ([FoodTreeNode Mass], Either [UnknownTree] (FullNode_ Mass))

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
            (fmap (PartialNode . PartialNode_ umh) . N.nonEmpty)
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
          (return $ FullNode_ diffMass umh [] (Left []))
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
  -> m (FullNode_ Mass)
fromNutTreeWithMass mass dn nt = do
  (ks, us) <- fromNutTreeWithMass_ mass nt
  return $ FullNode_ mass dn ks us

fromNutTreeWithoutMass
  :: NutrientState m
  => NutTree
  -> m ([FoodTreeNode Mass], NonEmpty UnknownTree)
fromNutTreeWithoutMass NutTree {ntFractions, ntUnmeasuredHeader, ntUnmeasuredTree} = do
  (umk, umu) <- case ntUnmeasuredTree of
    Nothing -> return (Nothing, [])
    Just ut -> bimap (fmap go . N.nonEmpty) toList <$> fromNutTreeWithoutMass ut

  (ks, us) <- either (second toList) ((,[]) . toList) <$> readBranches ntFractions

  return (maybe ks (: ks) umk, UnknownTree umn umu :| us)
  where
    umh = summedToDisplay ntUnmeasuredHeader
    umn = snName ntUnmeasuredHeader
    go = PartialNode . PartialNode_ umh

readBranches
  :: NutrientState m
  => Branches
  -> m
      ( Either
          ( [FoodTreeNode Mass]
          , NonEmpty UnknownTree
          )
          (NonEmpty (FoodTreeNode Mass))
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

    fromAgg (AggIdentity x) = fromHeader x
    fromAgg (Priority (x :| xs)) = do
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
        Just m -> Right . pure . FullNode <$> fromNutTreeWithMass m dh nt
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
        Just m -> Right $ pure $ FullNode $ FullNode_ m dh [] (Left [])
        Nothing -> Left ([], pure $ UnknownTree (dnName dh) [])

    fromKnowns _ [] = []
    fromKnowns dh (k : ks) = [PartialNode $ PartialNode_ dh (k :| ks)]

    fromUnknowns dh = pure . UnknownTree (dnName dh) . toList

    toKnownTree dh = pure . PartialNode . PartialNode_ dh

sumTrees :: Num a => NonEmpty (FoodTreeNode a) -> a
sumTrees (f :| fs) = foldr (\n -> (+ go n)) (go f) fs
  where
    go (FullNode n) = fnValue n
    go (PartialNode n) = sumTrees $ pnKnown n

summedToDisplay :: SummedNutrient -> DisplayNutrient
summedToDisplay (SummedNutrient x y) = DisplayNutrient x y

measToDisplay :: MeasuredNutrient -> DisplayNutrient
measToDisplay (Direct (DirectNutrient _ n p)) = DisplayNutrient n p
measToDisplay (Alternate (AltNutrient n p _)) = DisplayNutrient n p

entireTree
  :: NutrientState m
  => ProteinConversion
  -> m ([FoodTreeNode Mass], Either [UnknownTree] (FullNode_ Mass))
entireTree = fromNutTreeWithMass_ standardMass . nutHierarchy

fullToDisplayTree :: FullNodeData -> DisplayNode Mass
fullToDisplayTree = uncurry (toNode standardMass)
  where
    toNode v ks = uncurry (DisplayNode v) . unpackNodes v ks

    unpackNodes mass ks u = case u of
      Right fn -> (toMap (FullNode fn : ks), mempty)
      Left uts -> case N.nonEmpty ks of
        Nothing -> (mempty, withNonEmpty (`M.singleton` mass) mempty uts)
        Just ks' ->
          (toMap $ toList ks', M.singleton uts (mass - sumTrees ks'))

    partialToDisplayTree PartialNode_ {pnNut, pnKnown} =
      ( pnNut
      , DisplayNode (sumTrees pnKnown) (toMap $ toList pnKnown) mempty
      )

    fullToDisplayTree_ FullNode_ {fnNut, fnValue, fnKnown, fnUnknown} =
      (fnNut, toNode fnValue fnKnown fnUnknown)

    toMap = M.fromList . fmap go

    go (FullNode n) = fullToDisplayTree_ n
    go (PartialNode n) = partialToDisplayTree n

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
      :: (C.ToNamedRecord (DisplayRow g), C.DefaultOrdered (DisplayRow g))
      => NonEmpty (DisplayTree g)
      -> BL.ByteString
    go = C.encodeDefaultOrderedByNameWith eos . N.toList . sconcat . fmap (treeToRows tos)

-- TODO not sure how to get the instance constraints out of the rankN function
-- (they don't seem necessary)
chooseGrouping
  :: GroupOptions
  -> ( forall g
        . ( ToJSON g
          , C.DefaultOrdered (DisplayRow g)
          , C.ToNamedRecord (DisplayRow g)
          )
       => NonEmpty (DisplayTree g)
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

-- TODO add option to remove unknowns
treeToRows :: AllTabularDisplayOptions -> DisplayTree g -> NonEmpty (DisplayRow g)
treeToRows (AllTabularDisplayOptions su uu r) (DisplayTree_ (DisplayNode v ks us) e g) =
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

dumpNutrientTree :: [NutTreeRow]
dumpNutrientTree = goTree Nothing $ nutHierarchy 0
  where
    goTree parent (NutTree bs umh umt) =
      concatMap (goBranch parent) bs
        ++ [goSummed parent umh]
        ++ maybe [] (goTree parent) umt

    goBranch parent (AggIdentity n) = goNode parent n
    goBranch parent (Priority ns) = concatMap (goNode parent) ns

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
