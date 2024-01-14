module Internal.Nutrient where

import Data.Monoid
import Data.Scientific
import Internal.NutrientTree
import Internal.Nutrients
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import RIO.State

ingredientToTree
  :: MealState m
  => [Modification]
  -> Scientific
  -> FoodItem
  -> m FinalFood
ingredientToTree ms mass f =
  fmap (scaleNV scale) <$> foodItemToTree (foldr modifyItem f ms)
  where
    scale = mass / 100

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

-- TODO add branched (which will required forcing the data for the label into
-- a list of food nutrients)
foodItemToTree :: MealState m => FoodItem -> m FinalFood
foodItemToTree (Foundation f) = foundationToTree f
foodItemToTree (SRLegacy f) = legacyToTree f

legacyToTree :: MealState m => SRLegacyFoodItem -> m FinalFood
legacyToTree SRLegacyFoodItem {srlMeta, srlCommon} =
  legFoundToTree srlMeta srlCommon

foundationToTree :: MealState m => FoundationFoodItem -> m FinalFood
foundationToTree FoundationFoodItem {ffiMeta, ffiCommon} =
  legFoundToTree ffiMeta ffiCommon

legFoundToTree
  :: MealState m
  => FoodRequiredMeta
  -> FoundationLegacyCommon
  -> m FinalFood
legFoundToTree fm flc = do
  (t, stFin) <- runStateT (displayTree $ pcFactor pConv) st
  modify (fsWarnings stFin ++)
  let f = FinalFood_ t $ computeCalories cConv t
  return $ fmap (\v -> NutrientValue (Sum v) $ pure rd) f
  where
    rd = FoodMeta (frmDescription fm) (frmId fm)
    st = FoodState (fcFoodNutrients $ flcCommon flc) []
    pConv = flcProteinConversion flc
    cConv = flcCalorieConversion flc

-- TODO dummy value for protein smells funny
computeCalories :: CalorieConversion -> DisplayNode Scientific -> Scientific
computeCalories (CalorieConversion ff pf cf _) dn =
  ff * go (measToDisplay lipid)
    + pf * go (measToDisplay (protein 0))
    + cf * go (summedToDisplay carbDiff)
  where
    go n = fromMaybe 0 $ lookupTree n dn

scaleNV :: Num a => a -> NutrientValue_ (Sum a) -> NutrientValue_ (Sum a)
scaleNV x = fmap (fmap (* x))

divNV :: Integral n => NutrientValue -> n -> NutrientValue
divNV n d = fmap (fmap (`divSci` d)) n
