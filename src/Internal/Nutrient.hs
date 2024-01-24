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
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import RIO.State

-- TODO also not DRY
customToTree
  :: [Modification]
  -> Scientific
  -> CustomSource
  -> Either CustomIngError (FinalFood, NutrientMap)
customToTree ms mass s = do
  i <- customToItem s
  return $ first (fmap (scaleNV scale)) $ customItemToTree (foldr modifyItem i ms)
  where
    scale = mass / 100

-- TODO not dry
customItemToTree :: FoodItem () NutrientMap -> (FinalFood, NutrientMap)
customItemToTree (FoodItem _ d ns cc pc) =
  first go $ runState (displayTree pc) ns
  where
    go t = fmap toNV $ FinalFood_ t $ computeCalories cc t
    toNV v = NutrientValue (Sum v) $ pure $ FoodMeta d Nothing

customToItem :: CustomSource -> Either CustomIngError (FoodItem () NutrientMap)
customToItem
  CustomSource
    { scDesc
    , scRemainder
    , scRemainderPrefix
    , scNutrients
    , scCalorie
    , scProtein
    } =
    case N.nonEmpty dups of
      Just ds -> Left $ CustomDups scDesc ds
      Nothing
        | nutMass > standardMass -> Left $ TooMuchMass scDesc
        | otherwise -> return $ FoodItem () scDesc nutMap scCalorie (fromFloatDigits scProtein)
    where
      nuts = go <$> scNutrients
      nutMass = sum $ vnAmount . snd <$> nuts
      remNut = ValidNutrient (standardMass - nutMass) scRemainderPrefix
      remId = NID $ fromIntegral scRemainder
      dups = findDups $ remId : fmap fst nuts
      nutMap = M.fromList $ (remId, remNut) : nuts
      go CustomNutrient {cnID, cnMass, cnPrefix} =
        ( NID $ fromIntegral cnID
        , ValidNutrient (fromFloatDigits cnMass) cnPrefix
        )

data CustomIngError
  = CustomDups Text (NonEmpty NID)
  | TooMuchMass Text

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
    go (FoodNutrient (Just (Nutrient (Just i) (Just _) (Just u))) (Just v)) =
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
modifyItem :: Modification -> FoodItem i NutrientMap -> FoodItem i NutrientMap
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
    toNV v = NutrientValue (Sum v) $ pure $ FoodMeta d (Just i)

-- TODO dummy value for protein smells funny
computeCalories :: CalorieConversion -> DisplayNode Scientific -> Scientific
computeCalories (CalorieConversion ff pf cf) dn =
  fromFloatDigits ff * go (measToDisplay lipid)
    + fromFloatDigits pf * go (measToDisplay (protein 0))
    + fromFloatDigits cf * go (summedToDisplay carbDiff)
  where
    go n = fromMaybe 0 $ lookupTree n dn

scaleNV :: Num a => a -> NutrientValue_ (Sum a) -> NutrientValue_ (Sum a)
scaleNV x = fmap (fmap (* x))

-- divNV :: Integral n => NutrientValue -> n -> NutrientValue
-- divNV n d = fmap (fmap (`divSci` d)) n
