module Internal.Nutrient where

import Data.Monoid
import Data.Scientific
import Internal.Types.Dhall
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.List as L

-- calcTreeToRows :: CalculatedTree -> [RowNutrient]
-- calcTreeToRows CalculatedTree {fctProximates, fctEnergy, fctCarbDiff} =
--   [ RowNutrient energyId "calories" (getSum fctEnergy) kcal
--   , RowNutrient totalFatId "total fat" (getSum $ pTotal $ ptProtein fctProximates) g
--   , RowNutrient carbDiffId "total carbohydrates" (getSum fctCarbDiff) g
--   , RowNutrient proteinId "total protein" (getSum $ pTotal $ ptProtein fctProximates) g
--   ]
--   where
--     kcal = Unit Kilo Calorie
--     g = Unit Unity Gram

-- proxToRows :: ProximateTree Scientific -> [RowNutrient]
-- proxToRows ProximateTree {ptProtein, ptCarbohydrates, ptLipids, ptAsh, ptWater} =
--   [a]
--   where
--     a =

-- where
--   energy = RowNutrient ftId rnDesc
--   carbDiff = RowNutrient

ingredientToTree
  :: MonadAppError m
  => [Modification]
  -> Double
  -> FoodItem
  -> m FoodTree
ingredientToTree ms mass f =
  fmap (scaleNV scale) <$> foodItemToTree (foldr modifyItem f ms)
  where
    scale = fromFloatDigits mass / 100.0

modifyItem :: Modification -> FoodItem -> FoodItem
modifyItem m i = case i of
  Foundation f@FoundationFoodItem {ffiCommon} ->
    Foundation $ f {ffiCommon = go ffiCommon}
  SRLegacy f@SRLegacyFoodItem {srlCommon} ->
    SRLegacy $ f {srlCommon = go srlCommon}
  where
    go c0@FoundationLegacyCommon {flcCommon = c1@FoodCommon {fcFoodNutrients}} =
      c0 {flcCommon = c1 {fcFoodNutrients = modifyNutrient m <$> fcFoodNutrients}}

modifyNutrient :: Modification -> FoodNutrient -> FoodNutrient
modifyNutrient
  Modification {modNutID, modScale}
  f@FoodNutrient {fnId, fnAmount}
    | Just (fromIntegral modNutID) == fnId =
        f {fnAmount = (* fromFloatDigits modScale) <$> fnAmount}
    | otherwise = f

foodItemToTree :: MonadAppError m => FoodItem -> m FoodTree
foodItemToTree (Foundation f) = foundationToTree f
foodItemToTree (SRLegacy f) = legacyToTree f

legacyToTree :: MonadAppError m => SRLegacyFoodItem -> m FoodTree
legacyToTree SRLegacyFoodItem {srlMeta, srlCommon} =
  legFoundToTree srlMeta srlCommon

foundationToTree :: MonadAppError m => FoundationFoodItem -> m FoodTree
foundationToTree FoundationFoodItem {ffiMeta, ffiCommon} =
  legFoundToTree ffiMeta ffiCommon

legFoundToTree
  :: MonadAppError m
  => FoodRequiredMeta
  -> FoundationLegacyCommon
  -> m FoodTree
legFoundToTree fm flc = do
  ps <- nutrientsToTree pConv nuts
  return $
    FoodTree
      { ftName = frmDescription fm
      , ftId = frmId fm
      , ftCalculated = calculateTree cConv ps
      }
  where
    nuts = fcFoodNutrients $ flcCommon flc
    pConv = flcProteinConversion flc
    cConv = flcCalorieConversion flc

calculateTree
  :: CalorieConversion
  -> ProximateTree
  -> CalculatedTree
calculateTree
  CalorieConversion {ccFat, ccProtein, ccCarbs}
  t@ProximateTree
    { ptProtein = Proteins {pTotal}
    , ptLipids = Lipids {lTotal}
    , ptAsh = Ash {aTotal}
    , ptWater = water
    } = CalculatedTree t energy carbs
    where
      carbs =
        initNV $ 100.0 - val pTotal - val lTotal - val aTotal - val water
      energy =
        initNV $
          ccFat * val lTotal + ccProtein * val pTotal + ccCarbs * val carbs
      val = getSum . nvValue

nutrientsToTree
  :: MonadAppError m
  => ProteinConversion
  -> [FoodNutrient]
  -> m ProximateTree
nutrientsToTree pc ns = do
  p <- nutrientsToProteins pc ns
  c <- nutrientsToCarbs ns
  l <- nutrientsToLipids ns
  a <- nutrientsToAsh ns
  w <- lookupAmountError waterId ns
  return $ ProximateTree p c l a w

nutrientsToProteins
  :: MonadAppError m
  => ProteinConversion
  -> [FoodNutrient]
  -> m Proteins
nutrientsToProteins ProteinConversion {pcFactor} ns =
  Proteins <$> maybe fromNitrogen pure fromProtein
  where
    fromProtein = lookupAmount proteinId ns
    fromNitrogen = scaleNV pcFactor <$> lookupAmountError nitrogenId ns

nutrientsToCarbs :: MonadAppError m => [FoodNutrient] -> m Carbohydrates
nutrientsToCarbs ns = do
  f <- nutrientsToFiber ns
  pure $
    Carbohydrates
      (nutrientsToSugars ns)
      (nutrientsToOligos ns)
      f
      (lookupAmount starchId ns)

nutrientsToSugars :: [FoodNutrient] -> Sugars
nutrientsToSugars ns =
  Sugars
    (lookupAmount totalSugarsId ns)
    (lookupAmount sucroseId ns)
    (lookupAmount glucoseId ns)
    (lookupAmount fructoseId ns)
    (lookupAmount lactoseId ns)
    (lookupAmount maltoseId ns)
    (lookupAmount galactoseId ns)

nutrientsToOligos :: [FoodNutrient] -> OligoSaccharides
nutrientsToOligos ns =
  OligoSaccharides
    (lookupAmount raffinoseId ns)
    (lookupAmount stachyoseId ns)
    (lookupAmount verbascoseId ns)

nutrientsToFiber :: MonadAppError m => [FoodNutrient] -> m Fiber
nutrientsToFiber ns =
  Fiber
    <$> nutrientsTo1992Fiber ns
    <*> nutrientsTo2011Fiber ns
    <*> pure (lookupAmount betaGlucanId ns)

nutrientsTo1992Fiber
  :: MonadAppError m
  => [FoodNutrient]
  -> m (Maybe FiberFractions1992)
nutrientsTo1992Fiber =
  nutrientsToFiberFraction FiberFractions1992 totalFiber1992Id solublseFiberId insolublseFiberId

nutrientsTo2011Fiber
  :: MonadAppError m
  => [FoodNutrient]
  -> m (Maybe FiberFractions2011)
nutrientsTo2011Fiber =
  nutrientsToFiberFraction FiberFractions2011 totalFiber2011Id highMWFiberId lowMWFiberId

nutrientsToFiberFraction
  :: MonadAppError m
  => ( NutrientValue
       -> Maybe NutrientValue
       -> Maybe NutrientValue
       -> f NutrientValue
     )
  -> Int
  -> Int
  -> Int
  -> [FoodNutrient]
  -> m (Maybe (f NutrientValue))
nutrientsToFiberFraction c tid i0 i1 ns =
  case (lookupAmount i0 ns, lookupAmount i1 ns) of
    (Just r0, Just r1) -> do
      t <- lookupAmountError tid ns
      pure $ Just $ c t (Just r0) (Just r1)
    _ -> pure $ (\t -> c t Nothing Nothing) <$> lookupAmount tid ns

nutrientsToLipids :: MonadAppError m => [FoodNutrient] -> m Lipids
nutrientsToLipids ns = Lipids <$> lookupAmountError totalFatId ns

nutrientsToAsh :: MonadAppError m => [FoodNutrient] -> m Ash
nutrientsToAsh ns = do
  t <- lookupAmountError totalFatId ns
  pure $ Ash t (nutrientsToMinerals ns)

nutrientsToMinerals :: [FoodNutrient] -> Minerals
nutrientsToMinerals ns =
  Minerals
    (lookupAmount sodiumId ns)
    (lookupAmount magnesiumId ns)
    (lookupAmount phosphorusId ns)
    (lookupAmount potassiumId ns)
    (lookupAmount calciumId ns)
    (lookupAmount manganeseId ns)
    (lookupAmount ironId ns)
    (lookupAmount copperId ns)
    (lookupAmount zincId ns)
    (lookupAmount seleniumId ns)
    (lookupAmount molybdenumId ns)
    (lookupAmount iodineId ns)

energyId :: Int
energyId = 1008

carbDiffId :: Int
carbDiffId = 1005

waterId :: Int
waterId = 1051

nitrogenId :: Int
nitrogenId = 1002

proteinId :: Int
proteinId = 1003

starchId :: Int
starchId = 1009

totalSugarsId :: Int
totalSugarsId = 1063

sucroseId :: Int
sucroseId = 1010

glucoseId :: Int
glucoseId = 1011

fructoseId :: Int
fructoseId = 1012

lactoseId :: Int
lactoseId = 1013

maltoseId :: Int
maltoseId = 1014

galactoseId :: Int
galactoseId = 1075

raffinoseId :: Int
raffinoseId = 1076

stachyoseId :: Int
stachyoseId = 1077

verbascoseId :: Int
verbascoseId = 2063

totalFiber1992Id :: Int
totalFiber1992Id = 1079

solublseFiberId :: Int
solublseFiberId = 1082

insolublseFiberId :: Int
insolublseFiberId = 1084

totalFiber2011Id :: Int
totalFiber2011Id = 2033

highMWFiberId :: Int
highMWFiberId = 2038

lowMWFiberId :: Int
lowMWFiberId = 2065

betaGlucanId :: Int
betaGlucanId = 2058

totalFatId :: Int
totalFatId = 1004

ashId :: Int
ashId = 1007

calciumId :: Int
calciumId = 1087

ironId :: Int
ironId = 1089

magnesiumId :: Int
magnesiumId = 1090

phosphorusId :: Int
phosphorusId = 1091

potassiumId :: Int
potassiumId = 1092

sodiumId :: Int
sodiumId = 1093

zincId :: Int
zincId = 1095

copperId :: Int
copperId = 1098

iodineId :: Int
iodineId = 1100

manganeseId :: Int
manganeseId = 1101

molybdenumId :: Int
molybdenumId = 1102

seleniumId :: Int
seleniumId = 1103

findId :: Int -> [FoodNutrient] -> Maybe FoodNutrient
findId i = L.find (\x -> fnId x == Just i)

findIdError :: MonadAppError m => Int -> [FoodNutrient] -> m FoodNutrient
findIdError i = maybe (throwAppError $ NutrientError i) return . findId i

lookupAmount :: Int -> [FoodNutrient] -> Maybe NutrientValue
lookupAmount i = (fmap initNV . fnAmount) <=< findId i

lookupAmountError :: MonadAppError m => Int -> [FoodNutrient] -> m NutrientValue
lookupAmountError i = maybe (throwAppError $ NutrientError i) return . lookupAmount i

initNV :: Scientific -> NutrientValue
initNV s = NutrientValue (Sum s) 1

scaleNV :: Num a => a -> NutrientValue_ (Sum a) -> NutrientValue_ (Sum a)
scaleNV x = fmap (fmap (* x))
