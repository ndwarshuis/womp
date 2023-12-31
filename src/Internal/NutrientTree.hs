module Internal.NutrientTree
  ( nutHierarchy
  , displayTree
  , lookupTree
  , summedToDisplay
  , measToDisplay
  )
where

import Data.Monoid
import Data.Scientific
import Internal.Nutrients
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.Map as M
import qualified RIO.NonEmpty as N
import RIO.State
import qualified RIO.Text as T

findRemove :: (a -> Bool) -> [a] -> (Maybe a, [a])
findRemove f = go []
  where
    -- TODO this reverse isn't really necessary (this would only be necessary if
    -- I wanted more speed, in which case this would be an ordered set on which
    -- I could perform binary search, since that isn't the case, order doesn't
    -- matter)
    go acc [] = (Nothing, reverse acc)
    go acc (x : xs)
      | f x = (Just x, reverse acc ++ xs)
      | otherwise = go (x : acc) xs

type Grams = Scientific

-- TODO make these warnings actually say something
findMass :: NutrientState m => Int -> m (Maybe Grams)
findMass i = do
  f <- state getFoodNutrient
  maybe (return Nothing) go f
  where
    getFoodNutrient s =
      second (\x -> s {fsNutrients = x}) $
        findRemove (\x -> (nId =<< fnNutrient x) == Just i) (fsNutrients s)
    -- if food has a given nutrient but it has no amount or the wrong unit,
    -- skip and throw a warning since this is not supposed to happen (ideally)
    -- but won't necessarily compromise the final result
    go f = do
      let a = fnAmount f
      let u = parseUnit =<< nUnitName =<< fnNutrient f
      case (a, u) of
        (Just m, Just (Unit p Gram)) -> pure $ Just $ raisePower (prefixValue p) m
        (Just _, Just _) -> throwAppWarning NotGram >> return Nothing
        (Just _, Nothing) -> throwAppWarning NoUnit >> return Nothing
        (Nothing, _) -> throwAppWarning NoAmount >> return Nothing

findMeasured :: NutrientState m => MeasuredNutrient -> m (Maybe Grams)
findMeasured n = case n of
  Direct m -> findMass $ mnId m
  Alternate AltNutrient {anChoices} -> foldM go Nothing anChoices
  where
    go Nothing (i, s) = liftA2 (*) s <$> findMass i
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

nutHierarchy :: Scientific -> NutTree
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
    leaf = Single . Leaf
    -- TODO use the right unit here
    group n = Single . UnmeasuredHeader (SummedNutrient n Micro)
    measured h = Single . MeasuredHeader h
    unmeasured h = Single . UnmeasuredHeader h
    nutTree u xs =
      NutTree
        { ntFractions = xs
        , ntUnmeasuredHeader = u
        , ntUnmeasuredTree = Nothing
        }
    measuredLeaves h u xs = measured h $ nutTree u (leaf <$> xs)
    unmeasuredLeaves h xs = unmeasured h (leaf <$> xs)
    groupLeaves h xs = group h (leaf <$> xs)
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
      group "Organics" $
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
      group "Vitamins" $
        groupLeaves "Vitamin A" allVitaminA
          :| [ group "Vitamin B" $
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
                fmap leaf $
                  vitaminD2 :| [vitaminD3, calcifediol, vitaminD4]
             , groupLeaves "Vitamin E" allVitaminE
             , group "Vitamin K" $
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

-- nv :: NutrientReader m => Grams -> m NutrientValue
-- nv m = (NutrientValue (Sum m) . pure) <$> ask

fromNutTreeWithMass
  :: NutrientState m
  => Scientific
  -> DisplayNutrient
  -> NutTree
  -> m (FullNode_ Scientific)
fromNutTreeWithMass mass dn NutTree {ntFractions, ntUnmeasuredHeader, ntUnmeasuredTree} = do
  -- possible results from this operation:
  -- 1) all known -> we know the mass of the unmeasured tree
  -- 2) at least one unknown -> we don't know the mass of the unknown tree
  res <- readBranches ntFractions
  let toNode k u = FullNode_ mass dn k u
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
      return $ toNode (maybe ks (: ks) umk) (Left $ UnknownTree umh umus : toList ms)
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
      return $ toNode (toList ks) $ Right um
  where
    umh = summedToDisplay ntUnmeasuredHeader

fromNutTreeWithoutMass
  :: NutrientState m
  => NutTree
  -> m ([FoodTreeNode Scientific], NonEmpty UnknownTree)
fromNutTreeWithoutMass NutTree {ntFractions, ntUnmeasuredHeader, ntUnmeasuredTree} = do
  (umk, umu) <- case ntUnmeasuredTree of
    Nothing -> return (Nothing, [])
    Just ut -> bimap (fmap go . N.nonEmpty) toList <$> fromNutTreeWithoutMass ut

  (ks, us) <- either (second toList) ((,[]) . toList) <$> readBranches ntFractions

  return (maybe ks (: ks) umk, UnknownTree umh umu :| us)
  where
    umh = summedToDisplay ntUnmeasuredHeader
    go = PartialNode . PartialNode_ umh

readBranches
  :: NutrientState m
  => Branches
  -> m
      ( Either
          ( [FoodTreeNode Scientific]
          , NonEmpty UnknownTree
          )
          (NonEmpty (FoodTreeNode Scientific))
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

    fromAgg (Single x) = fromHeader x
    fromAgg (Priority (x :| xs)) = do
      init <- fromHeader x
      foldM go init xs
      where
        go (Left ([], _)) next = fromHeader next
        go acc _ = return acc

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
        Nothing -> Left ([], pure $ UnknownTree dh [])

    fromKnowns _ [] = []
    fromKnowns dh (k : ks) = [PartialNode $ PartialNode_ dh (k :| ks)]

    fromUnknowns dh = pure . UnknownTree dh . toList

    toKnownTree dh = pure . PartialNode . PartialNode_ dh

sumTrees :: NonEmpty (FoodTreeNode Scientific) -> Scientific
sumTrees (f :| fs) = foldr (\n -> (+ go n)) (go f) fs
  where
    go (FullNode n) = fnValue n
    go (PartialNode n) = sumTrees $ pnKnown n

summedToDisplay :: SummedNutrient -> DisplayNutrient
summedToDisplay (SummedNutrient x y) = DisplayNutrient x y

measToDisplay :: MeasuredNutrient -> DisplayNutrient
measToDisplay (Direct (DirectNutrient _ n p)) = DisplayNutrient n p
measToDisplay (Alternate (AltNutrient n p _)) = DisplayNutrient n p

entireTree :: NutrientState m => Scientific -> m (FullNode_ Scientific)
entireTree = fromNutTreeWithMass 100 (DisplayNutrient "All" Unity) . nutHierarchy

fullToDisplayTree :: FullNode_ Scientific -> (DisplayNutrient, DisplayNode Scientific)
fullToDisplayTree FullNode_ {fnValue, fnNut, fnKnown, fnUnknown} =
  (fnNut, uncurry (DisplayNode fnValue) $ unpackNodes fnValue fnKnown fnUnknown)
  where
    unpackNodes mass ks u = case u of
      Left uts -> case N.nonEmpty ks of
        Nothing -> (mempty, M.singleton uts mass)
        Just ks' ->
          (toMap $ toList ks', M.singleton uts $ sumTrees ks')
      Right fn -> (toMap (FullNode fn : ks), mempty)

    partialToDisplayTree PartialNode_ {pnNut, pnKnown} =
      ( pnNut
      , DisplayNode (sumTrees pnKnown) (toMap $ toList pnKnown) mempty
      )

    toMap = M.fromList . fmap go

    go (FullNode n) = fullToDisplayTree n
    go (PartialNode n) = partialToDisplayTree n

displayTree :: NutrientState m => Scientific -> m (DisplayNutrient, DisplayNode Scientific)
displayTree = fmap fullToDisplayTree . entireTree

-- TODO not sure how efficient this is; probably doesn't matter than much ;)
lookupTree :: DisplayNutrient -> DisplayNode a -> Maybe a
lookupTree k (DisplayNode _ ks _) =
  msum ((dnValue <$> M.lookup k ks) : (lookupTree k <$> M.elems ks))
