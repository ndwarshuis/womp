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
  :: [Modification]
  -> Scientific
  -> MappedFoodItem
  -> (FinalFood, NutrientMap)
ingredientToTree ms mass f =
  first (fmap (scaleNV scale)) $ foodItemToTree (foldr modifyItem f ms)
  where
    scale = mass / 100

mapFoodItem :: ParsedFoodItem -> ([NutrientWarning], MappedFoodItem)
mapFoodItem f@FoodItem {fiFoodNutrients = ns} =
  second (\ns' -> f {fiFoodNutrients = M.fromList ns'}) $
    partitionEithers $
      fmap go ns
  where
    go (FoodNutrient (Just (Nutrient (Just i) (Just n) (Just u))) (Just v)) =
      case parseUnit u of
        Just (Unit p Gram) ->
          Right (i, ValidNutrient (raisePower (prefixValue p) v) n p)
        Just _ -> Left $ NotGram i u
        Nothing -> Left $ UnknownUnit i u
    go n = Left $ InvalidNutrient n

-- TODO warn user when modifications don't match
modifyItem :: Modification -> MappedFoodItem -> MappedFoodItem
modifyItem Modification {modNutID, modScale} f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = M.adjust go (fromIntegral modNutID) ns}
  where
    go n@ValidNutrient {vnAmount} =
      n {vnAmount = vnAmount * fromFloatDigits modScale}

foodItemToTree :: MappedFoodItem -> (FinalFood, NutrientMap)
foodItemToTree (FoodItem i d ns cc pc) =
  first go $ runState (displayTree $ pcFactor pc) ns
  where
    go t = fmap toNV $ FinalFood_ t $ computeCalories cc t
    toNV v = NutrientValue (Sum v) $ pure $ FoodMeta d i

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

-- divNV :: Integral n => NutrientValue -> n -> NutrientValue
-- divNV n d = fmap (fmap (`divSci` d)) n
