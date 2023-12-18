module Internal.Nutrient where

import Data.Scientific
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.List as L

foundationToTree :: MonadAppError m => FoundationFoodItem -> m FoodTree
foundationToTree FoundationFoodItem {ffiMeta, ffiCommon} = do
  ps <- nutrientsToTree pConv nuts
  return $
    FoodTree
      { ftName = frmDescription ffiMeta
      , ftId = frmId ffiMeta
      , ftCalculated = calculateTree cConv ps
      }
  where
    nuts = fcFoodNutrients $ flcCommon ffiCommon
    pConv = flcProteinConversion ffiCommon
    cConv = flcCalorieConversion ffiCommon

calculateTree :: CalorieConversion -> ProximateTree -> CalculatedTree
calculateTree
  CalorieConversion {ccFat, ccProtein, ccCarbs}
  t@ProximateTree
    { ptProtein = Proteins {pTotal}
    , ptLipids = Lipids {lTotal}
    , ptAsh = Ash {aTotal}
    , ptWater = water
    } = CalculatedTree t energy carbs
    where
      carbs = 100.0 - pTotal - lTotal - aTotal - water
      energy = ccFat * lTotal + ccProtein * pTotal + ccCarbs * carbs

nutrientsToTree
  :: MonadAppError m
  => ProteinConversion
  -> [FoodNutrient]
  -> m ProximateTree
nutrientsToTree pc ns = do
  p <- nutrientsToProteins pc ns
  l <- nutrientsToLipids ns
  a <- nutrientsToAsh ns
  w <- lookupAmountError waterId ns
  return $ ProximateTree p c l a w
  where
    c = nutrientsToCarbs ns

nutrientsToProteins :: MonadAppError m => ProteinConversion -> [FoodNutrient] -> m Proteins
nutrientsToProteins ProteinConversion {pcFactor} ns =
  Proteins <$> maybe fromNitrogen pure fromProtein
  where
    fromProtein = lookupAmount proteinId ns
    fromNitrogen = (* pcFactor) <$> lookupAmountError nitrogenId ns

waterId :: Int
waterId = 1051

nitrogenId :: Int
nitrogenId = 1002

proteinId :: Int
proteinId = 1003

nutrientsToCarbs :: [FoodNutrient] -> Carbohydrates
nutrientsToCarbs ns =
  Carbohydrates
    (nutrientsToSugars ns)
    (nutrientsToOligos ns)
    (nutrientsToFiber ns)
    (lookupAmount starchId ns)

starchId :: Int
starchId = 1009

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

nutrientsToFiber :: [FoodNutrient] -> Fiber
nutrientsToFiber ns =
  Fiber
    (lookupAmount totalFiberId ns)
    (nutrientsToPrecipitatedFiber ns)
    (lookupAmount betaGlucanId ns)
    (nutrientsToWeightedFiber ns)

nutrientsToPrecipitatedFiber :: [FoodNutrient] -> Maybe FiberPrecipitated
nutrientsToPrecipitatedFiber ns = do
  s <- lookupAmount solublseFiberId ns
  i <- lookupAmount insolublseFiberId ns
  pure $ FiberPrecipitated s i

nutrientsToWeightedFiber :: [FoodNutrient] -> Maybe FiberWeighted
nutrientsToWeightedFiber ns = do
  h <- lookupAmount highMWFiberId ns
  l <- lookupAmount lowMWFiberId ns
  pure $ FiberWeighted h l

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

totalFiberId :: Int
totalFiberId = 1079

solublseFiberId :: Int
solublseFiberId = 1082

insolublseFiberId :: Int
insolublseFiberId = 1084

highMWFiberId :: Int
highMWFiberId = 2038

lowMWFiberId :: Int
lowMWFiberId = 2065

betaGlucanId :: Int
betaGlucanId = 2058

nutrientsToLipids :: MonadAppError m => [FoodNutrient] -> m Lipids
nutrientsToLipids ns = Lipids <$> lookupAmountError totalFatId ns

totalFatId :: Int
totalFatId = 1004

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

lookupAmount :: Int -> [FoodNutrient] -> Maybe Scientific
lookupAmount i = fnAmount <=< findId i

lookupAmountError :: MonadAppError m => Int -> [FoodNutrient] -> m Scientific
lookupAmountError i = maybe (throwAppError $ NutrientError i) return . lookupAmount i
