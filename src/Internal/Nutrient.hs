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
import qualified RIO.Set as S
import RIO.State

-- TODO make sure mass is less than 100
-- TODO make sure all ids unique
customToTree
  :: [Modification]
  -> Scientific
  -> CustomSource
  -> FinalFood
customToTree
  ms
  mass
  CustomSource {scDesc, scRemainder, scRemainderPrefix, scNutrients, scCalorie, scProtein} = undefined
    where
      h = nutHierarchy (fromFloatDigits scProtein)
      nm = customMap scNutrients
      nmMod = foldr modifyMap nm ms
      nutMass = sum $ fmap vnAmount $ M.elems nm
      nid = (NID $ fromIntegral scRemainder)
      rem = (ValidNutrient (standardMass - nutMass) scRemainderPrefix)
      nmFinal = M.insert nid rem nmMod

customMap :: [CustomNutrient] -> NutrientMap
customMap = M.fromList . fmap go
  where
    go CustomNutrient {cnID, cnMass, cnPrefix} =
      ( NID $ fromIntegral cnID
      , ValidNutrient (fromFloatDigits cnMass) cnPrefix
      )

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
          Right (i, ValidNutrient (raisePower (prefixValue p) v) p)
        Just _ -> Left $ NotGram i u
        Nothing -> Left $ UnknownUnit i u
    go n = Left $ InvalidNutrient n

filterFoodItem :: ParsedFoodItem -> ParsedFoodItem
filterFoodItem f@FoodItem {fiFoodNutrients = ns} =
  f {fiFoodNutrients = filter go ns}
  where
    go fi = maybe True (\i -> not $ S.member i ignoredNutrients) (nId =<< fnNutrient fi)

-- TODO warn user when modifications don't match
modifyItem :: Modification -> MappedFoodItem -> MappedFoodItem
modifyItem m f = f {fiFoodNutrients = modifyMap m $ fiFoodNutrients f}

modifyMap :: Modification -> NutrientMap -> NutrientMap
modifyMap Modification {modNutID, modScale} = M.adjust go (fromIntegral modNutID)
  where
    go n@ValidNutrient {vnAmount} =
      n {vnAmount = vnAmount * fromFloatDigits modScale}

foodItemToTree :: MappedFoodItem -> (FinalFood, NutrientMap)
foodItemToTree (FoodItem i d ns cc pc) =
  first go $ runState (displayTree pc) ns
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
