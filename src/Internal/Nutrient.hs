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
import qualified RIO.Map as M
import RIO.State

ingredientToTree
  :: MealState m
  => [Modification]
  -> Scientific
  -> MappedFoodItem
  -> m FinalFood
ingredientToTree ms mass f =
  fmap (scaleNV scale) <$> foodItemToTree (foldr modifyItem f ms)
  where
    scale = mass / 100

mapFoodItem :: ParsedFoodItem -> ([NutrientWarning], MappedFoodItem)
mapFoodItem f@FoodItem {fiFoodNutrients = ns} =
  let (ws, ns') = partitionEithers $ fmap go ns
   in (ws, f {fiFoodNutrients = M.fromList ns'})
  where
    go (FoodNutrient (Just (Nutrient (Just i) (Just n) (Just u))) (Just v)) =
      case parseUnit u of
        Just (Unit p Gram) ->
          Right $ (i, ValidNutrient (raisePower (prefixValue p) v) n p)
        Just _ -> Left $ NotGram i u
        Nothing -> Left $ UnknownUnit i u
    go n = Left $ InvalidNutrient n

modifyItem :: Modification -> MappedFoodItem -> MappedFoodItem
modifyItem m f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = modifyNutrient m <$> ns}

modifyNutrient :: Modification -> FoodNutrient -> FoodNutrient
modifyNutrient
  Modification {modNutID, modScale}
  f@FoodNutrient {fnAmount}
    | Just modNutID == undefined =
        f {fnAmount = (* fromFloatDigits modScale) <$> fnAmount}
    | otherwise = f

foodItemToTree :: MealState m => MappedFoodItem -> m FinalFood
foodItemToTree (FoodItem i d ns cc pc) = do
  (t, stFin) <- runStateT (displayTree $ pcFactor pc) st
  modify (fsWarnings stFin ++)
  let f = FinalFood_ t $ computeCalories cc t
  return $ fmap (\v -> NutrientValue (Sum v) $ pure rd) f
  where
    rd = FoodMeta d i
    st = FoodState ns []

-- TODO dummy value for protein smells funny
computeCalories :: CalorieConversion -> DisplayNode Scientific -> Scientific
computeCalories (CalorieConversion ff pf cf) dn =
  ff * go (measToDisplay lipid)
    + pf * go (measToDisplay (protein 0))
    + cf * go (summedToDisplay carbDiff)
  where
    go n = fromMaybe 0 $ lookupTree n dn

scaleNV :: Num a => a -> NutrientValue_ (Sum a) -> NutrientValue_ (Sum a)
scaleNV x = fmap (fmap (* x))

divNV :: Integral n => NutrientValue -> n -> NutrientValue
divNV n d = fmap (fmap (`divSci` d)) n
