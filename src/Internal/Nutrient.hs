module Internal.Nutrient where

import Data.Monoid
import Data.Scientific
import Internal.Types.Dhall
import Internal.Types.Main
import Internal.Utils
import RIO
import RIO.State

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
  -> m CalculatedTree
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

foodItemToTree :: MonadAppError m => FoodItem -> m CalculatedTree
foodItemToTree (Foundation f) = foundationToTree f
foodItemToTree (SRLegacy f) = legacyToTree f

legacyToTree :: MonadAppError m => SRLegacyFoodItem -> m CalculatedTree
legacyToTree SRLegacyFoodItem {srlMeta, srlCommon} =
  legFoundToTree srlMeta srlCommon

foundationToTree :: MonadAppError m => FoundationFoodItem -> m CalculatedTree
foundationToTree FoundationFoodItem {ffiMeta, ffiCommon} =
  legFoundToTree ffiMeta ffiCommon

legFoundToTree
  :: MonadAppError m
  => FoodRequiredMeta
  -> FoundationLegacyCommon
  -> m CalculatedTree
legFoundToTree fm flc =
  flip runReaderT rd $ do
    -- TODO do something with the state somehow (ie print the remaining nutrients
    -- that didn't get parsed in a debug message)
    (ps, _) <- runStateT (nutrientsToTree pConv) nuts
    calculateTree cConv ps
  where
    rd = FoodMeta (frmId fm) (frmDescription fm)
    nuts = fcFoodNutrients $ flcCommon flc
    pConv = flcProteinConversion flc
    cConv = flcCalorieConversion flc

calculateTree
  :: NutrientReader m
  => CalorieConversion
  -> ProximateTree
  -> m CalculatedTree
calculateTree
  CalorieConversion {ccFat, ccProtein, ccCarbs}
  t@ProximateTree
    { ptProtein = Proteins {pTotal}
    , ptLipids = Lipids {lTotal}
    , ptAsh = AshFraction {aTotal}
    , ptWater = water
    } = do
    carbs <-
      initNV $ 100.0 - val pTotal - val lTotal - val aTotal - val water
    energy <-
      initNV $
        ccFat * val lTotal + ccProtein * val pTotal + ccCarbs * val carbs
    return $ CalculatedTree t energy carbs
    where
      val = getSum . nvValue

nutrientsToTree
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => ProteinConversion
  -> m ProximateTree
nutrientsToTree pc = do
  p <- nutrientsToProteins pc
  c <- nutrientsToCarbs
  l <- nutrientsToLipids
  a <- nutrientsToAsh
  w <- lookupAmountError Water
  return $ ProximateTree p c l a w

nutrientsToProteins
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => ProteinConversion
  -> m Proteins
nutrientsToProteins ProteinConversion {pcFactor} =
  Proteins <$> (maybe fromNitrogen (return . scaleNV pcFactor) =<< fromProtein)
  where
    fromProtein = lookupAmount Protein
    fromNitrogen = lookupAmountError Nitrogen

nutrientsToCarbs :: (NutrientReader m, NutrientState m, MonadAppError m) => m Carbohydrates
nutrientsToCarbs =
  Carbohydrates
    <$> nutrientsToSugars
    <*> nutrientsToOligos
    <*> nutrientsToFiber
    <*> lookupAmount Starch

nutrientsToSugars :: (NutrientReader m, NutrientState m) => m Sugars
nutrientsToSugars =
  Sugars
    <$> lookupAmount TotalSugar
    <*> lookupAmount Sucrose
    <*> lookupAmount Glucose
    <*> lookupAmount Fructose
    <*> lookupAmount Lactose
    <*> lookupAmount Maltose
    <*> lookupAmount Galactose

nutrientsToOligos :: (NutrientReader m, NutrientState m) => m OligoSaccharides
nutrientsToOligos =
  OligoSaccharides
    <$> lookupAmount Raffinose
    <*> lookupAmount Stachyose
    <*> lookupAmount Verbascose

nutrientsToFiber :: (NutrientReader m, NutrientState m, MonadAppError m) => m Fiber
nutrientsToFiber =
  Fiber
    <$> nutrientsTo1992Fiber
    <*> nutrientsTo2011Fiber
    <*> lookupAmount BetaGlucan

nutrientsTo1992Fiber
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => m (Maybe FiberFractions1992)
nutrientsTo1992Fiber =
  nutrientsToFiberFraction FiberFractions1992 TotalFiber1992 SolublseFiber InsolublseFiber

nutrientsTo2011Fiber
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => m (Maybe FiberFractions2011)
nutrientsTo2011Fiber =
  nutrientsToFiberFraction FiberFractions2011 TotalFiber2011 HighMWFiber LowMWFiber

nutrientsToFiberFraction
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => ( NutrientValue
       -> Maybe NutrientValue
       -> Maybe NutrientValue
       -> f NutrientValue
     )
  -> AppNutrient
  -> AppNutrient
  -> AppNutrient
  -> m (Maybe (f NutrientValue))
nutrientsToFiberFraction c tid i0 i1 = do
  a0 <- lookupAmount i0
  a1 <- lookupAmount i1
  case (a0, a1) of
    (Just r0, Just r1) -> do
      t <- lookupAmountError tid
      pure $ Just $ c t (Just r0) (Just r1)
    _ -> mapM (\t -> pure $ c t Nothing Nothing) =<< lookupAmount tid

nutrientsToLipids :: (NutrientReader m, NutrientState m, MonadAppError m) => m Lipids
nutrientsToLipids = Lipids <$> lookupAmountError TotalFat

nutrientsToAsh :: (NutrientReader m, NutrientState m, MonadAppError m) => m AshFraction
nutrientsToAsh =
  AshFraction <$> lookupAmountError Ash <*> nutrientsToMinerals

nutrientsToMinerals :: (NutrientReader m, NutrientState m) => m Minerals
nutrientsToMinerals =
  Minerals
    <$> lookupAmount Sodium
    <*> lookupAmount Magnesium
    <*> lookupAmount Phosphorus
    <*> lookupAmount Potassium
    <*> lookupAmount Calcium
    <*> lookupAmount Manganese
    <*> lookupAmount Iron
    <*> lookupAmount Copper
    <*> lookupAmount Zinc
    <*> lookupAmount Selenium
    <*> lookupAmount Molybdenum
    <*> lookupAmount Iodine

-- energyId :: Int
-- energyId = 1008

-- carbDiffId :: Int
-- carbDiffId = 1005

-- waterId :: Int
-- waterId = 1051

-- nitrogenId :: Int
-- nitrogenId = 1002

-- proteinId :: Int
-- proteinId = 1003

-- starchId :: Int
-- starchId = 1009

-- totalSugarsId :: Int
-- totalSugarsId = 1063

-- sucroseId :: Int
-- sucroseId = 1010

-- glucoseId :: Int
-- glucoseId = 1011

-- fructoseId :: Int
-- fructoseId = 1012

-- lactoseId :: Int
-- lactoseId = 1013

-- maltoseId :: Int
-- maltoseId = 1014

-- galactoseId :: Int
-- galactoseId = 1075

-- raffinoseId :: Int
-- raffinoseId = 1076

-- stachyoseId :: Int
-- stachyoseId = 1077

-- verbascoseId :: Int
-- verbascoseId = 2063

-- totalFiber1992Id :: Int
-- totalFiber1992Id = 1079

-- solublseFiberId :: Int
-- solublseFiberId = 1082

-- insolublseFiberId :: Int
-- insolublseFiberId = 1084

-- totalFiber2011Id :: Int
-- totalFiber2011Id = 2033

-- TODO nitrogen = 1002
-- highMWFiberId :: Int
-- highMWFiberId = 2038

-- lowMWFiberId :: Int
-- lowMWFiberId = 2065

-- betaGlucanId :: Int
-- betaGlucanId = 2058

-- totalFatId :: Int
-- totalFatId = 1004

-- ashId :: Int
-- ashId = 1007

-- calciumId :: Int
-- calciumId = 1087

-- ironId :: Int
-- ironId = 1089

-- magnesiumId :: Int
-- magnesiumId = 1090

-- phosphorusId :: Int
-- phosphorusId = 1091

-- potassiumId :: Int
-- potassiumId = 1092

-- sodiumId :: Int
-- sodiumId = 1093

-- zincId :: Int
-- zincId = 1095

-- copperId :: Int
-- copperId = 1098

-- iodineId :: Int
-- iodineId = 1100

-- manganeseId :: Int
-- manganeseId = 1101

-- molybdenumId :: Int
-- molybdenumId = 1102

-- seleniumId :: Int
-- seleniumId = 1103

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

findId :: Int -> [FoodNutrient] -> (Maybe FoodNutrient, [FoodNutrient])
findId i = go []
  where
    -- TODO this reverse isn't really necessary (this would only be necessary if
    -- I wanted more speed, in which case this would be an ordered set on which
    -- I could perform binary search, since that isn't the case, order doesn't
    -- matter)
    go acc [] = (Nothing, reverse acc)
    go acc (x : xs)
      | fnId x == Just i = (Just x, reverse acc ++ xs)
      | otherwise = go (x : acc) xs

-- findId i = L.find (\x -> fnId x == Just i)

findIdM :: NutrientState m => Int -> m (Maybe FoodNutrient)
findIdM i = state (findId i)

-- findIdError :: (NutrientState m, MonadAppError m) => AppNutrient -> m FoodNutrient
-- findIdError i = maybe (throwAppError $ NutrientError $ Meas i) return =<< findIdM i

lookupVal :: (NutrientReader m, NutrientState m) => n -> m (Maybe Scientific)
lookupVal i = (fnAmount =<<) <$> findIdM (toId i)

lookupValError
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => n
  -> m (Maybe Scientific)
lookupValError i = (fnAmount =<<) <$> findIdM (toId i)

lookupAmount :: (NutrientReader m, NutrientState m) => n -> m (Maybe NutrientValue)
lookupAmount i = mapM initNV =<< lookupVal i

lookupAmountError
  :: (NutrientReader m, NutrientState m, MonadAppError m)
  => n
  -> m NutrientValue
lookupAmountError i =
  maybe (throwAppError $ NutrientError $ Meas i) return =<< lookupAmount i

initNV :: (Displayable n, NutrientReader m) => Scientific -> m NutrientValue
initNV s = NutrientValue (Sum s) . pure . Disp <$> ask

scaleNV :: Num a => a -> NutrientValue_ (Sum a) -> NutrientValue_ (Sum a)
scaleNV x = fmap (fmap (* x))

type NutrientReader r m = MonadReader FoodMeta

type NutrientState = MonadState [FoodNutrient]
