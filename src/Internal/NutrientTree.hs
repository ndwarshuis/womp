module Internal.NutrientTree
  ( nutHierarchy
  , displayTree
  , lookupTree
  , summedToDisplay
  , measToDisplay
  , fmtFullTree
  , nodesToRows
  , finalToJSON
  )
where

import Data.Aeson
import Data.Monoid
import Data.Scientific
import Data.Semigroup
import Internal.Nutrients
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import RIO.State
import qualified RIO.Text as T
import RIO.Time

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

fmtFullTree :: DisplayOptions -> SpanFood NutrientMass NutrientEnergy -> T.Text
fmtFullTree o (SpanFood (FinalFood m (NutrientValue (Sum v) _)) (start, end)) =
  T.unlines
    [ T.unwords ["Start:", tshow start]
    , T.unwords ["End:", tshow $ addDays (fromIntegral end) start]
    , T.unwords ["Energy:", tshow v, "kcal"]
    , fmtTree o m
    ]

fmtTree :: DisplayOptions -> DisplayNode NutrientMass -> T.Text
fmtTree o@(DisplayOptions u _ _ uy) (DisplayNode v ks us) = T.unlines (header : rest)
  where
    header = T.unwords ["Total mass:", tshow $ getSum $ nvValue v, "g"]
    rest = go ks us
    go ks' us' =
      withMap fmtKnown ks' ++ if u then withMap fmtUnknown us' else fmtUnknownTotal us'

    withMap f = concatMap (uncurry f) . M.assocs

    addIndent n = T.append (T.replicate (2 * n) " ")

    fmtKnown dn (DisplayNode v' ks' us') =
      fmtHeader (fmtDisplayNutrient o dn v') $ go ks' us'

    fmtUnknownTotal =
      maybeToList . fmap (fmtUnknownHeader . sconcat) . N.nonEmpty . M.elems

    fmtUnknown ts v' =
      let h = fmtUnknownHeader v'
       in fmtHeader h $ if u then concatMap fmtUnknownTree ts else []

    fmtUnknownHeader (NutrientValue (Sum v') _) =
      let (p, v'') = unityMaybeSci uy (autoPrefix $ unMass v') (unMass v')
       in T.concat
            [ "Unknown: "
            , tshow v''
            , " "
            , tunit $ Unit p Gram
            ]

    fmtUnknownTree (UnknownTree n ts) =
      fmtHeader n $ concatMap fmtUnknownTree ts

    fmtHeader h = (h :) . fmap (addIndent 1)

fmtDisplayNutrient :: DisplayOptions -> DisplayNutrient -> NutrientMass -> T.Text
fmtDisplayNutrient
  DisplayOptions {doUnityUnits}
  (DisplayNutrient n p)
  (NutrientValue (Sum v) _) =
    T.concat [n, ": ", tshow v', " ", tunit (Unit p' Gram)]
    where
      (p', v') = unityMaybeSci doUnityUnits p $ unMass v

finalToJSON :: DisplayOptions -> SpanFood NutrientMass NutrientEnergy -> Value
finalToJSON o (SpanFood (FinalFood ms c) (start, end)) =
  object
    [ "start" .= start
    , "end" .= addDays (fromIntegral end) start
    , "energy" .= object ["value" .= c, "unit" .= Unit Kilo Calorie]
    , "mass" .= treeToJSON o "Total" Unity ms
    ]

treeToJSON
  :: DisplayOptions
  -> Text
  -> Prefix
  -> DisplayNode NutrientMass
  -> Value
treeToJSON o@(DisplayOptions u m e uy) n p (DisplayNode v ks us) =
  object $
    [ encodeValue v'
    , "name" .= n
    , encodeUnit p'
    , "known" .= mapK ks
    ]
      ++ ["unknown" .= mapU us | u]
  where
    (p', v') = unityMaybe uy p (fmap (fmap unMass) v)

    mapK = fmap (\(DisplayNutrient n' p'', v'') -> treeToJSON o n' p'' v'') . M.toList

    mapU = fmap (uncurry goUnk) . M.toList

    goUnk uts v'' =
      let (p'', v''') =
            unityMaybe uy (autoPrefix $ unMass $ getSum $ nvValue v'') (fmap (fmap unMass) v'')
       in object
            [ encodeValue v'''
            , encodeUnit p''
            , "trees" .= uts
            ]

    encodeValue v'' = if m then "value" .= v'' else "value" .= getSum (nvValue v'')
    encodeUnit p'' =
      let u' = Unit p'' Gram
       in if e then "unit" .= u' else "unit" .= tunit u'

-- | Convert a value and its prefix depending on if we want all values to be
-- displayed in their native units or all in unity (ie "grams" with no prefix).
-- Assume the incoming value is in grams
unityMaybe
  :: Bool
  -> Prefix
  -> NutrientValue_ (Sum Scientific)
  -> (Prefix, NutrientValue_ (Sum Scientific))
unityMaybe unity p n@(NutrientValue (Sum v) _) =
  let (p', v') = unityMaybeSci unity p v
   in (p', n {nvValue = Sum v'})

unityMaybeSci :: Bool -> Prefix -> Scientific -> (Prefix, Scientific)
unityMaybeSci True _ v = (Unity, v)
unityMaybeSci False p v = (p, raisePower (-prefixValue p) v)

nodesToRows :: SpanFood NutrientMass NutrientEnergy -> [DisplayRow]
nodesToRows (SpanFood (FinalFood (DisplayNode v ks us) e) (start, end)) =
  [energy, totalMass] ++ goK massName ks ++ [goU massName us]
  where
    end' = addDays (fromIntegral end) start

    dpyRow n pnt v' u =
      DisplayRow start end' n pnt (raisePower (-(prefixValue $ unitBase u)) v') u

    energy = dpyRow "Energy" Nothing (unEnergy $ val e) (Unit Kilo Calorie)

    totalMass = dpyRow "Total Mass" Nothing (unMass $ val v) (Unit Unity Gram)

    massRow n pnt v' p = dpyRow n (Just pnt) v' (Unit p Gram)

    massName = "Total Mass"

    unknownName = "Unknown"

    val = getSum . nvValue

    goK pnt = concatMap (uncurry (goK_ pnt)) . M.assocs

    goK_ pnt (DisplayNutrient n p) (DisplayNode v' ks' us') =
      massRow n pnt (unMass $ val v') p : (goK n ks' ++ [goU n us'])

    goU pnt us' =
      let s = unMass $ sum $ val <$> M.elems us'
          p = autoPrefix s
       in massRow unknownName pnt s p
