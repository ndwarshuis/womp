module Internal.Nutrients
  ( nutHierarchy
  , standardMass
  , ignoredNutrients
  -- used for computing calories
  , lipid
  , dispProtein
  , carbDiff
  , totalMass
  )
where

import Internal.Types.FoodItem
import Internal.Types.Main
import Internal.Utils
import RIO
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import qualified RIO.Text as T

standardMass :: Mass
standardMass = 100

--------------------------------------------------------------------------------
-- proximates

totalMass :: DisplayNutrient
totalMass = DisplayNutrient "Total Mass" Unity

water :: MeasuredNutrient
water = Direct $ DirectNutrient 1051 "Water" Unity

dispProtein :: DisplayNutrient
dispProtein = DisplayNutrient "Protein" Unity

protein :: ProteinConversion -> MeasuredNutrient
protein n2Factor =
  Alternate $
    AltNutrient (dnName dispProtein) (dnPrefix dispProtein) $
      (proteinId, 1) :| [(nitrogenId, unPC n2Factor)]
  where
    nitrogenId = 1002
    proteinId = 1003

lipid :: MeasuredNutrient
lipid = Direct $ DirectNutrient 1004 "Lipids" Unity

ash :: MeasuredNutrient
ash = Direct $ DirectNutrient 1007 "Ash" Unity

carbDiff :: SummedNutrient
carbDiff = SummedNutrient "Carbohydrates (by difference)" Unity

otherCarbs :: SummedNutrient
otherCarbs = SummedNutrient "Other Carbs" Unity

otherLipids :: SummedNutrient
otherLipids = SummedNutrient "Other Lipids" Unity

otherProteinMass :: SummedNutrient
otherProteinMass = SummedNutrient "Other Protein Mass" Milli

otherInorganics :: SummedNutrient
otherInorganics = SummedNutrient "Other Inorganics" Unity

--------------------------------------------------------------------------------
-- sterols

cholesterol :: MeasuredNutrient
cholesterol = Direct $ DirectNutrient 1253 "Cholesterol" Unity

phytosterols :: MeasuredNutrient
phytosterols = Direct $ DirectNutrient 1283 "Phytosterols" Unity

allPhytosterols :: NonEmpty MeasuredNutrient
allPhytosterols =
  listDirect Micro $
    -- this doesn't strictly seem like a phytosterol but is still included in
    -- the same section along with the rest
    (2053, "Stigmastadiene")
      :| [ (1285, "Stigmasterol")
         , (1286, "Campesterol")
         , (1287, "Brassicasterol")
         , (1288, "BetaSitosterol")
         , (1289, "Campestanol")
         , (1294, "BetaSitostanol")
         , (1296, "Delta5Avenasterol")
         , (2052, "Delta7Stigmastenol")
         , (1284, "Ergosterol")
         , (2060, "Ergosta-7-enol")
         , (2061, "Ergosta-7,22-dienol")
         , (2062, "Ergosta-5,7-dienol")
         ]

-- TODO not sure how to use this, I already calculate "unknown phytosterols"
-- within the tree construction by default (see summed nutrient below)
-- otherPhytosterols :: MeasuredNutrient
-- otherPhytosterols = Direct $ DirectNutrient 1298 "Other Phytosterols" Unity

otherPhytosterols :: SummedNutrient
otherPhytosterols = SummedNutrient "Other Phytosterols" Unity

--------------------------------------------------------------------------------
-- fatty acids (enter if you dare)
--
-- general nomenclature rules (as far as I can decode) for PUFAs and MUFAs:
-- - no suffix = toplevel
-- - c = cis isomers
-- - t = trans isomers (and distinct from c even though c sometimes means "good trans")
-- - i = other isomers (not cis and not anything else that might be measured)
-- - nX = omega X fatty acid (all bonds assumed cis and therefore under c)
--
-- NOTE: these rules do not always seem to be followed in the database, so they
-- might be wrong (or the database has typos in it). For example, sometimes the
-- "no suffix" amount is the same as the "c" amount even though there is a "t"
-- amount; however sometimes "c" and "t" add up to "no suffix", which is what
-- I would expect.

tfas :: MeasuredNutrient
tfas = Direct $ DirectNutrient 1257 "Trans Fatty Acids" Unity

monoTFAs :: MeasuredNutrient
monoTFAs = Direct $ DirectNutrient 1329 "Trans Fatty acids (monoenoic)" Unity

diTFAs :: MeasuredNutrient
diTFAs = Direct $ DirectNutrient 1330 "Trans Fatty acids (dienoic)" Unity

polyTFAs :: MeasuredNutrient
polyTFAs = Direct $ DirectNutrient 1331 "Trans Fatty acids (polyenoic)" Unity

otherMonoTFAs :: SummedNutrient
otherMonoTFAs = SummedNutrient "Trans Fatty acids (other monoenoic)" Unity

otherDiTFAs :: SummedNutrient
otherDiTFAs = SummedNutrient "Trans Fatty acids (other dienoic)" Unity

otherPolyTFAs :: SummedNutrient
otherPolyTFAs = SummedNutrient "Trans Fatty acids (other polyenoic)" Unity

allMonoTFAs :: NonEmpty MeasuredNutrient
allMonoTFAs = listDirect Unity $ fmap (second go) ids
  where
    ids =
      (1281, 14 :: Int)
        :| [ (1303, 16)
           , (2011, 17)
           , (1304, 18)
           , -- , (1306, (18, 2))
             -- , (2019, (18, 3))
             (2013, 20)
           , (1305, 22)
           ]
    go = tfaName . (,1)

tfa_18_2 :: MeasuredNutrient
tfa_18_2 = Direct $ DirectNutrient 1306 (tfaName (18, 2)) Unity

tfa_18_3 :: MeasuredNutrient
tfa_18_3 = Direct $ DirectNutrient 2019 (tfaName (18, 3)) Unity

tfaName :: FACarbons -> Text
tfaName (carbons, bonds) = T.concat ["TFA ", tshow carbons, ":", tshow bonds]

mufas :: MeasuredNutrient
mufas = Direct $ DirectNutrient 1292 "Monounsaturated Fatty Acids" Unity

pufas :: MeasuredNutrient
pufas = Direct $ DirectNutrient 1293 "Polyunsaturated Fatty Acids" Unity

sfas :: MeasuredNutrient
sfas = Direct $ DirectNutrient 1258 "Saturated Fatty Acids" Unity

otherSFAs :: SummedNutrient
otherSFAs = SummedNutrient "Other SFAs" Unity

otherTFAs :: SummedNutrient
otherTFAs = SummedNutrient "Other TFAs" Unity

otherMUFAs :: SummedNutrient
otherMUFAs = SummedNutrient "Other MUFAs" Unity

otherPUFAs :: SummedNutrient
otherPUFAs = SummedNutrient "Other PUFAs" Unity

allSFAs :: NonEmpty MeasuredNutrient
allSFAs = listDirect Unity $ fmap (second go) ids
  where
    ids =
      (1259, 4 :: Int)
        :| [ (2003, 5)
           , (1260, 6)
           , (2004, 7)
           , (1261, 8)
           , (2005, 9)
           , (1262, 10)
           , (1335, 11)
           , (1263, 12)
           , (1332, 13)
           , (1264, 14)
           , (1299, 15)
           , (1265, 16)
           , (1300, 17)
           , (1266, 18)
           , (1267, 20)
           , (2006, 21)
           , (1273, 22)
           , (2007, 23)
           , (1301, 24)
           ]
    go carbons = T.concat ["SFA ", tshow carbons, ":0"]

mufa_12_1 :: MeasuredNutrient
mufa_12_1 = mufa 12 2008 Nothing

-- NOTE: it seems most of the MUFAs are not characterized further other than
-- number of carbons. In this case, several of the MUFAs are redundant (ie the
-- one's that end in c and the ones that don't). Rather than have multiple
-- levels for each, collapse them into single layer to keep my code less lame

mufa_14_1 :: NutrientChoice Node
mufa_14_1 = toMUFA 14 (FATree 2009 []) (Just (FATree 1274 []))

mufa_15_1 :: MeasuredNutrient
mufa_15_1 = mufa 15 1333 Nothing

mufa_16_1 :: NutrientChoice Node
mufa_16_1 = toMUFA 16 (FATree 1275 []) (Just (FATree 1314 []))

mufa_17_1 :: NutrientChoice Node
mufa_17_1 = toMUFA 17 (FATree 1323 []) (Just (FATree 2010 []))

mufa_18_1 :: NutrientChoice Node
mufa_18_1 = toMUFA 18 (FATree 1268 []) (Just cis)
  where
    cis = FATree 1315 [(1412, "11t")] -- must be an important trans fat...

mufa_20_1 :: NutrientChoice Node
mufa_20_1 = toMUFA 20 (FATree 1277 []) (Just (FATree 2012 []))

mufa_22_1 :: NutrientChoice Node
mufa_22_1 = toMUFA 22 (FATree 1279 []) (Just cis)
  where
    cis = FATree 1317 [(2014, "omega-9 (Erucic Acid)"), (2015, "22:1 omega-11")]

mufa_24_1 :: MeasuredNutrient
mufa_24_1 = mufa 24 1312 Nothing

pufa_18_2 :: NutrientChoice Node
pufa_18_2 = toUFA (18, 2) total (Just cis)
  where
    -- NOTE: i = uncommon isomers, mostly trans (I think)
    total = FATree 1269 [(1307, "(uncommon isomers)")]
    cis =
      FATree
        2016
        [ (1311, "(Conjugated Linoleic Acids)")
        , (1316, "omega-6 c,c (Linoleic Acid)")
        ]

pufa_18_3 :: NutrientChoice Node
pufa_18_3 = toUFA (18, 3) total (Just cis)
  where
    total = FATree 1270 [(1409, "(uncommon isomers)")]
    cis =
      FATree
        2018
        [ (1321, "omega-6 c,c,c (Gamma-linolenic Acid)")
        , (1404, "omega-3 c,c,c (Alpha-linolenic Acid)")
        ]

pufa_18_4 :: MeasuredNutrient
pufa_18_4 = pufa (18, 4) 1276 Nothing

-- NOTE: this is only listed as 20:2c (ie only cis-isomers without the
-- cis+trans category)
pufa_20_2 :: NutrientChoice Node
pufa_20_2 = toCisUFA (20, 2) $ FATree 2026 [(1313, "omega-6 c,c")]

pufa_20_3 :: NutrientChoice Node
pufa_20_3 = toUFA (20, 3) total (Just cis)
  where
    total = FATree 1325 []
    cis =
      FATree
        2020
        [ (1405, "omega-3 c,c,c (Eicosatetraenoic Acid)")
        , (1406, "omega-6 c,c,c (Dihomo-gamma-linolenic Acid)")
        , (1414, "omega-9 c,c,c (Mead Acid)")
        ]

pufa_20_4 :: NutrientChoice Node
pufa_20_4 = toUFA (20, 4) total (Just cis)
  where
    total = FATree 1271 []
    cis = FATree 2022 [(1408, "omega-6")]

-- NOTE this has no cis+trans analog above it
pufa_20_5 :: NutrientChoice Node
pufa_20_5 = toCisUFA (20, 5) cis
  where
    cis = FATree 2023 [(1278, "omega-3")]

pufa_22_2 :: MeasuredNutrient
pufa_22_2 = pufa (22, 2) 1334 Nothing

pufa_22_3 :: MeasuredNutrient
pufa_22_3 = pufa (22, 3) 2021 Nothing

pufa_22_4 :: MeasuredNutrient
pufa_22_4 = pufa (22, 4) 1411 Nothing

pufa_22_5 :: NutrientChoice Node
pufa_22_5 = toCisUFA (22, 5) cis
  where
    cis = FATree 2024 [(1280, "omega-3 c,c,c,c,c (Docosapentaenoic Acid)")]

pufa_22_6 :: NutrientChoice Node
pufa_22_6 = toCisUFA (22, 6) cis
  where
    cis = FATree 2025 [(1272, "omega-3 c,c,c,c,c,c (Docosahexaenoic Acid)")]

toCisUFA :: FACarbons -> FATree -> NutrientChoice Node
toCisUFA uc (FATree cisID subIDs) = case N.nonEmpty subIDs of
  Nothing -> leaf cis
  Just sids -> measuredLeaves cis other $ fmap (uncurry go) sids
  where
    cis = pufa uc cisID (Just "(cis isomers)")
    other = summedUfa uc "(other cis isomers)"
    go i n = pufa uc i (Just n)

ufaName :: FACarbons -> Text
ufaName (carbons, 1) = T.concat ["MUFA ", tshow carbons, ":1"]
ufaName (carbons, bonds) = T.concat ["PUFA ", tshow carbons, ":", tshow bonds]

ufa :: FACarbons -> NID -> Maybe Text -> MeasuredNutrient
ufa uc i extra = Direct $ DirectNutrient i (T.append n t) Unity
  where
    n = ufaName uc
    t = maybe "" (T.append " ") extra

pufa :: FACarbons -> NID -> Maybe Text -> MeasuredNutrient
pufa = ufa

mufa :: Int -> NID -> Maybe Text -> MeasuredNutrient
mufa carbons = pufa (carbons, 1)

summedUfa :: FACarbons -> Text -> SummedNutrient
summedUfa uc t = SummedNutrient (T.unwords [n, t]) Unity
  where
    n = ufaName uc

data FATree = FATree
  { fatParent :: NID
  , fatChildren :: [(NID, Text)]
  }

type FACarbons = (Int, Int)

toMUFA :: Int -> FATree -> Maybe FATree -> NutrientChoice Node
toMUFA carbons = toUFA (carbons, 1)

toUFA :: FACarbons -> FATree -> Maybe FATree -> NutrientChoice Node
toUFA uc FATree {fatParent, fatChildren} cis =
  case N.nonEmpty $ maybeToList (toCisUFA uc <$> cis) ++ nonCis of
    Nothing -> leaf total
    Just children -> measured total $ nutTree other children
  where
    total = pufa uc fatParent Nothing
    other = summedUfa uc "(unclassified)"
    go i n = leaf $ pufa uc i (Just n)
    nonCis = uncurry go <$> fatChildren

--------------------------------------------------------------------------------
-- carbohydrates

-- TODO what to do with Carbohydrates, Other (1072) or Sugar alcohols (1086)

-- the alt ID is just an older ID for for the same thing (I think), also quite
-- rare in the database it seems so not that important
betaGlucan :: MeasuredNutrient
betaGlucan = Alternate $ AltNutrient "Beta Glucans" Unity $ (2058, 1) :| [(1068, 1)]

starch :: MeasuredNutrient
starch = Direct $ DirectNutrient 1009 "Starch" Unity

fiberBySolubility :: MeasuredNutrient
fiberBySolubility = Direct $ DirectNutrient 1079 "Soluble/Insoluble Fiber" Unity

fiberByWeight :: MeasuredNutrient
fiberByWeight = Direct $ DirectNutrient 2033 "High/Low Molecular Weight Fiber" Unity

otherFiberBySolubility :: SummedNutrient
otherFiberBySolubility = SummedNutrient "Other Fiber (unclassified solubility)" Unity

otherFiberByWeight :: SummedNutrient
otherFiberByWeight = SummedNutrient "Other Fiber (unclassified weight)" Unity

highMWFiber :: MeasuredNutrient
highMWFiber = Direct $ DirectNutrient 2038 "High Molecular Weight Fiber" Unity

lowMWFiber :: MeasuredNutrient
lowMWFiber = Direct $ DirectNutrient 2065 "Low Molecular Weight Fiber" Unity

solubleFiber :: MeasuredNutrient
solubleFiber = Direct $ DirectNutrient 1082 "Soluble Fiber" Unity

insolubleFiber :: MeasuredNutrient
insolubleFiber = Direct $ DirectNutrient 1084 "Insoluble Fiber" Unity

-- 1235 = added sugar, 1236 = "intrinsic" sugar (which usually won't be present,
-- so this will just be a summation of whatever is beneath it)
totalSugars :: MeasuredNutrient
totalSugars =
  Alternate $ AltNutrient "Total Sugars" Unity $ (1235, 1) :| [(1236, 1), (2000, 1)]

allSugars :: NonEmpty MeasuredNutrient
allSugars =
  listDirect Unity $
    (1010, "Sucrose")
      :| [ (1011, "Glucose")
         , (1012, "Fructose")
         , (1013, "Lactose")
         , (1014, "maltose")
         , (1075, "Galactose")
         , (1076, "Raffinose")
         , (1077, "Stachyose")
         , (2063, "Verbascose")
         ]

otherSugars :: SummedNutrient
otherSugars = SummedNutrient "Other Sugars" Unity

-- TODO add inulin (1403)
-- TODO add lignin (1080)

--------------------------------------------------------------------------------
-- amino acids

allAminoAcids :: NonEmpty MeasuredNutrient
allAminoAcids =
  listDirect
    Milli
    $ (1210, "Tryptophan")
      :| [ (1211, "Threonine")
         , (1212, "Isoleucine")
         , (1213, "Leucine")
         , (1214, "Lysine")
         , (1215, "Methionine")
         , (1216, "Cystine")
         , (1217, "Phenylalanine")
         , (1218, "Tyrosine")
         , (1219, "Valine")
         , (1220, "Arginine")
         , (1221, "Histidine")
         , (1222, "Alanine")
         , (1223, "Aspartic Acid")
         , (1224, "Glutamic Acid")
         , (1225, "Glycine")
         , (1226, "Proline")
         , (1227, "Serine")
         , (1228, "Hydroxyproline")
         , (1231, "Asparagine")
         , (1232, "Cysteine")
         , (1233, "Glutamine")
         ]

--------------------------------------------------------------------------------
-- minerals

allMinerals :: NonEmpty MeasuredNutrient
allMinerals = append simple [na, cl]
  where
    simple =
      listDirect Milli $
        (1087, "Calcium")
          :| [ (1089, "Iron")
             , (1090, "Magnesium")
             , (1091, "Phosphorus")
             , (1092, "Potassium")
             , (1094, "Sulfur")
             , (1095, "Zinc")
             , (1096, "Chromium")
             , (1097, "Cobalt")
             , (1098, "Copper")
             , (1100, "Iodine")
             , (1099, "Fluoride")
             , (1101, "Manganese")
             , (1102, "Molybdenum")
             , (1103, "Selenium")
             , (1137, "Boron")
             , (1146, "Nickel")
             ]

    na = fromSalt "Sodium" 1093 saltConversion

    -- TODO since this actually says "chlorine" and not "chloride" I'm not sure
    -- if this salt conversion really counts.
    cl = fromSalt "Clorine" 1088 (1 - saltConversion)

    fromSalt n i c = Alternate $ AltNutrient n Milli $ (i, 1) :| [(1149, c)]

    saltConversion = 22.990 / (22.990 + 35.45)

--------------------------------------------------------------------------------
-- vitamins

allVitaminA :: NonEmpty MeasuredNutrient
allVitaminA =
  listDirect Micro $
    (1105, "Retinol")
      :| [ (1108, "alpha-carotene")
         , (1107, "beta-carotene")
         , (1159, "cis-beta-carotene")
         , (2028, "trans-beta-carotene")
         , (1118, "gamma-carotene")
         , (2032, "alpha-carotene")
         , (1120, "beta-carotene")
         ]

vitaminB1 :: MeasuredNutrient
vitaminB1 = Direct $ DirectNutrient 1165 "Vitamin B1 (thiamine)" Milli

vitaminB2 :: MeasuredNutrient
vitaminB2 = Direct $ DirectNutrient 1166 "Vitamin B2 (riboflavin)" Milli

vitaminB3 :: MeasuredNutrient
vitaminB3 = Direct $ DirectNutrient 1167 "Vitamin B3 (niacin)" Milli

vitaminB5 :: MeasuredNutrient
vitaminB5 = Direct $ DirectNutrient 1170 "Vitamin B5 (Pantathenic acid)" Milli

vitaminB6 :: MeasuredNutrient
vitaminB6 = Direct $ DirectNutrient 1175 "Vitamin B6 (pyridoxine)" Milli

vitaminB7 :: MeasuredNutrient
vitaminB7 = Direct $ DirectNutrient 1176 "Vitamin B7 (biotin)" Milli

dfeFolate :: MeasuredNutrient
dfeFolate =
  Linear $
    LinearNutrient (Just 1190) "Vitamin B9 (dietary folate equivalents)" Micro $
      fromAcidAndFood :| [fromTotalAndFood, fromTotalAndAcid]
  where
    -- DFE = 1.7 * folic acid + 1.0 * folate from food
    -- total folate = folic acid + folate from food
    fromAcidAndFood = (foodId, 1) :| [(acidId, 1.7)]
    fromTotalAndFood = (totalId, 1.7) :| [(foodId, -0.7)]
    fromTotalAndAcid = (totalId, 1) :| [(acidId, 0.7)]
    foodId = 1187
    acidId = 1186
    totalId = totalFolateId

totalFolateId :: NID
totalFolateId = 1177

totalFolate :: MeasuredNutrient
totalFolate = Direct $ DirectNutrient totalFolateId "Vitamin B9 (total folate)" Micro

-- TODO add 5-Formyltetrahydrofolic acid (1192), 10-Formyl folic acid (1191,
-- whatever this actually is), and 5-methyl tetrahydrofolate (1188)

vitaminB12 :: MeasuredNutrient
vitaminB12 =
  Alternate $
    AltNutrient "Vitamin B12 (cobalamins)" Micro $
      -- The alternative is "added B12" which I will just assume is "close
      -- enough" to the first ID since it doesn't break B12 down into specific
      -- molecular species.
      (1178, 1) :| [(1246, 1)]

vitaminC :: MeasuredNutrient
vitaminC = Direct $ DirectNutrient 1162 "Vitamin C (ascorbic acid)" Milli

vitaminD :: SummedNutrient
vitaminD = SummedNutrient "Vitamin D (total)" Micro

vitaminD2andD3 :: MeasuredNutrient
vitaminD2andD3 = Direct $ DirectNutrient 1114 "Vitamin D2+D3" Micro

-- | Vitamin D produced by mushrooms exposed to UV light
vitaminD2 :: MeasuredNutrient
vitaminD2 = Direct $ DirectNutrient 1111 "Vitamin D2 (ergocalciferol)" Micro

-- | Vitamin D made in human skin and found in some food
vitaminD3 :: MeasuredNutrient
vitaminD3 = Direct $ DirectNutrient 1112 "Vitamin D3 (cholecalciferol)" Micro

-- | hydroxylated D3, the stuff found in blood that a D3 blood test measures,
-- also found in some food
calcifediol :: MeasuredNutrient
calcifediol = Direct $ DirectNutrient 1113 "Vitamin D3 (calcifediol)" Micro

-- | Vitamin D found in certain mushrooms
vitaminD4 :: MeasuredNutrient
vitaminD4 = Direct $ DirectNutrient 2059 "Vitamin D4 (22-dihydroergocalciferol)" Micro

otherVitaminD :: SummedNutrient
otherVitaminD = SummedNutrient "Vitamin D (unclassified)" Micro

-- TODO what to do with just Vitamin E (1124, 1158, 2068)?

-- TODO if one really wants to get nerdy we could weight these by affinity for
-- the vitamin E transport receptor (see wikipedia article)
allVitaminE :: NonEmpty MeasuredNutrient
allVitaminE =
  listDirect Micro $
    (1109, "Vitamin E (alpha-Tocopherol)")
      :| [ (1125, "Vitamin E (beta-Tocopherol)")
         , (1126, "Vitamin E (gamma-Tocopherol)")
         , (1127, "Vitamin E (delta-Tocopherol)")
         , (1128, "Vitamin E (alpha-Tocotrienol)")
         , (1129, "Vitamin E (beta-Tocotrienol)")
         , (1130, "Vitamin E (gamma-Tocotrienol)")
         , (1131, "Vitamin E (delta-Tocotrienol)")
         , -- NOTE this is (probably) different from "normal" vitamin E since
           -- synthetic tocopherol is usually acetylated for stability, so
           -- technically this is a different species
           (1242, "Vitamin E (synthetic)")
         ]

vitaminK1 :: MeasuredNutrient
vitaminK1 = Direct $ DirectNutrient 1185 "Vitamin K1 (Phylloquinone)" Micro

-- | Note that this is synthetic and largely given to chickens, so is likely the
-- least important isomer of "K2". The others are produced by bacteria and are
-- present in many fermented products, unfortunately these don't seem to be
-- present in the DB (see https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3321250/)
vitaminK2 :: MeasuredNutrient
vitaminK2 = Direct $ DirectNutrient 1183 "Vitamin K2 (Menaquinone-4)" Micro

-- | A partially hydrogenated form of K1 (if this sounds bad, that's because it
-- is; its a byproduct of nuking oils to give them longer shelf life) (see cite
-- above)
dihydrophylloquinone :: MeasuredNutrient
dihydrophylloquinone = Direct $ DirectNutrient 1184 "Vitamin K1 (Dihydrophylloquinone)" Micro

--------------------------------------------------------------------------------
-- other random organics

lycopene :: MeasuredNutrient
lycopene = Direct $ DirectNutrient 1122 "Lycopene" Micro

cisLycopene :: MeasuredNutrient
cisLycopene = Direct $ DirectNutrient 1160 "cis-Lycopene" Micro

transLycopene :: MeasuredNutrient
transLycopene = Direct $ DirectNutrient 2029 "trans-Lycopene" Micro

-- | There isn't a good name for these, this really just means "lutein isomers"
-- even though the names sound like totally different compounds
luteins :: MeasuredNutrient
luteins = Direct $ DirectNutrient 1123 "Lutein and Zeaxanthin" Micro

transLutein :: MeasuredNutrient
transLutein = Direct $ DirectNutrient 1121 "trans-Lutein" Micro

-- | This is actually writen in the DB as "cis-Lutein/Zeaxanthin" but it is
-- summed with plain "zeaxanthin" as if it is only cis-Lutein, so I will call it
-- "cis-Lutein"
cisLutein :: MeasuredNutrient
cisLutein = Direct $ DirectNutrient 1161 "cis-Lutein" Micro

zeaxanthin :: MeasuredNutrient
zeaxanthin = Direct $ DirectNutrient 1119 "Zeaxanthin" Micro

choline :: MeasuredNutrient
choline = Direct $ DirectNutrient 1180 "Choline" Milli

allCholine :: NonEmpty MeasuredNutrient
allCholine =
  listDirect Milli $
    (1194, "Choline (unbound)")
      :| [ (1195, "Choline (phosphocholine)")
         , (1196, "Choline (phosphotidyl-choline)")
         , (1197, "Choline (glycerophospho-choline)")
         , (1199, "Choline (sphingomyelin)")
         ]

otherCholine :: SummedNutrient
otherCholine = SummedNutrient "Choline (unclassified)" Milli

-- | The phytoestrogens every bro who eats soy is worried about
isoflavones :: SummedNutrient
isoflavones = SummedNutrient "Isoflavones" Milli

allIsoflavones :: NonEmpty MeasuredNutrient
allIsoflavones =
  listDirect Milli $
    (1340, "Daidzein")
      :| [ (2049, "Daidzin")
         , (1341, "Genistein")
         , (2050, "Genistin")
         , (2051, "Glycitin")
         ]

-- | I'm assuming this means "trimethylglycine" since "betaine" actually refers
-- to a class of molecules that seem quite different
betaine :: MeasuredNutrient
betaine = Direct $ DirectNutrient 1198 "Betaine" Milli

ethanol :: MeasuredNutrient
ethanol = Direct $ DirectNutrient 1018 "Ethanol" Milli

aceticAcid :: MeasuredNutrient
aceticAcid = Direct $ DirectNutrient 1026 "Acetic Acid" Milli

citricAcid :: MeasuredNutrient
citricAcid = Direct $ DirectNutrient 1032 "Citric Acid" Milli

lacticAcid :: MeasuredNutrient
lacticAcid = Direct $ DirectNutrient 1038 "Lactic Acid" Milli

malicAcid :: MeasuredNutrient
malicAcid = Direct $ DirectNutrient 1039 "Malic Acid" Milli

oxalicAcid :: MeasuredNutrient
oxalicAcid = Direct $ DirectNutrient 1041 "Oxalic Acid" Milli

pyruvicAcid :: MeasuredNutrient
pyruvicAcid = Direct $ DirectNutrient 1043 "Pyruvic Acid" Milli

quinicAcid :: MeasuredNutrient
quinicAcid = Direct $ DirectNutrient 1044 "Quinic Acid" Milli

caffeine :: MeasuredNutrient
caffeine = Direct $ DirectNutrient 1057 "Caffeine" Milli

theobromine :: MeasuredNutrient
theobromine = Direct $ DirectNutrient 1058 "Theobromine" Milli

epigallocatechin3gallate :: MeasuredNutrient
epigallocatechin3gallate =
  Direct $ DirectNutrient 1368 "Epigallocatechin-3-gallate" Milli

totalSugarAlcohols :: MeasuredNutrient
totalSugarAlcohols = Direct $ DirectNutrient 1086 "Total Sugar Alcohols" Milli

sorbitol :: MeasuredNutrient
sorbitol = Direct $ DirectNutrient 1056 "Sorbitol" Milli

xylitol :: MeasuredNutrient
xylitol = Direct $ DirectNutrient 1078 "Xylitol" Milli

inositol :: MeasuredNutrient
inositol = Direct $ DirectNutrient 1181 "Inositol" Milli

otherSugarAlcohols :: SummedNutrient
otherSugarAlcohols = SummedNutrient "Other Sugar Alcohols" Unity

taurine :: MeasuredNutrient
taurine = Direct $ DirectNutrient 1234 "Taurine" Milli

ergothioneine :: MeasuredNutrient
ergothioneine = Direct $ DirectNutrient 2057 "Ergothioneine" Milli

phytoene :: MeasuredNutrient
phytoene = Direct $ DirectNutrient 1116 "Phytoene" Milli

phytofluene :: MeasuredNutrient
phytofluene = Direct $ DirectNutrient 1117 "Phytofluene" Milli

glutathione :: MeasuredNutrient
glutathione = Direct $ DirectNutrient 2069 "Glutathione" Milli

--------------------------------------------------------------------------------
-- the tree

-- | Nutrients that are calculated which we don't need because we calculate them
-- on-the-fly ourselves or because they are not masses (IU or something else)
-- Note: these were determined by looking at the full Oct 2023 csv download and
-- finding nutrients that had no "method," which I took to mean that they were
-- calculated from other things and thus do not need to be parsed from the JSON
-- blob here. The vast majority of nutrients from this list were not included,
-- since it was not clear if these would be added in future releases, in which
-- case I want the user to be notified in the debug output that they are missing
-- so that can be added to the tree.
ignoredNutrients :: Set NID
ignoredNutrients =
  S.fromList
    [ 1005 -- carbs by difference
    , 1050 -- carbs by summation
    , 1104 -- vitamin A (IU)
    , 2039 -- carbs
    , 2040 -- other carotenoids
    , 2041 -- tocopherols and tocotrienols
    , 2042 -- amino acids
    , 2043 -- minerals
    , 2044 -- lipids
    , 2045 -- proximates
    , 2046 -- vitamins and other compounds
    , 2047 -- energy (general)
    , 2048 -- energy (specific)
    , 2054 -- total tocotrienols
    , 2055 -- total tocopherols
    , 2064 -- oligosaccharides
    , 2067 -- total vitamin A
    , 2068 -- total vitamin E
    , 1025 -- organic acids
    , 1063 -- total sugar
    , 1085 -- total fat
    , 1106 -- vitamin A (RAE)
    , 1008 -- energy
    , 1062 -- energy (joules)
    , 1110 -- Vitamin D2+D3 (IU)
    , 1329 -- lipids trans-mono
    , 1330 -- lipids trans-di
    ]

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
                      :| [ measured tfas $
                            nutTree
                              otherTFAs
                              ( measuredLeaves monoTFAs otherMonoTFAs allMonoTFAs
                                  :| [ measuredLeaves diTFAs otherDiTFAs $ pure tfa_18_2
                                     , measuredLeaves polyTFAs otherPolyTFAs $ pure tfa_18_3
                                     ]
                              )
                         , measuredLeaves sfas otherSFAs allSFAs
                         , measured pufas pufas_
                         , measured mufas mufas_
                         , measuredLeaves phytosterols otherPhytosterols allPhytosterols
                         ]
                  )
             ]
    , ntUnmeasuredHeader = carbDiff
    , ntUnmeasuredTree = Just carbs
    }
  where
    carbs =
      nutTree otherCarbs $
        leaf starch
          :| [ leaf betaGlucan
             , measuredLeaves totalSugars otherSugars allSugars
             , fiber
             , vitamins
             , organics
             , sugarAlcohols
             ]

    fiber =
      NutrientMany
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
             , leaf ethanol
             , leaf citricAcid
             , leaf lacticAcid
             , leaf aceticAcid
             , leaf malicAcid
             , leaf oxalicAcid
             , leaf pyruvicAcid
             , leaf quinicAcid
             , leaf taurine
             , leaf ergothioneine
             , leaf phytoene
             , leaf phytofluene
             , leaf glutathione
             , leaf caffeine
             , leaf theobromine
             , leaf epigallocatechin3gallate
             ]

    sugarAlcohols =
      measuredLeaves totalSugarAlcohols otherSugarAlcohols $
        sorbitol :| [xylitol, inositol]

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
                     , vitaminB9
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

    vitaminB9 = NutrientMany (Leaf dfeFolate :| [Leaf totalFolate])

    mufas_ =
      nutTree otherMUFAs $
        leaf mufa_12_1
          :| [ mufa_14_1
             , leaf mufa_15_1
             , mufa_16_1
             , mufa_17_1
             , mufa_18_1
             , mufa_20_1
             , mufa_22_1
             , leaf mufa_24_1
             ]

    pufas_ =
      nutTree otherPUFAs $
        pufa_18_2
          :| [ pufa_18_3
             , leaf pufa_18_4
             , pufa_20_2
             , pufa_20_3
             , pufa_20_4
             , pufa_20_5
             , leaf pufa_22_2
             , leaf pufa_22_3
             , leaf pufa_22_4
             , pufa_22_5
             , pufa_22_6
             ]

--------------------------------------------------------------------------------
-- misc functions

leaf :: MeasuredNutrient -> NutrientChoice Node
leaf = NutrientSingle . Leaf

group :: Text -> Prefix -> Branches -> NutrientChoice Node
group n p = NutrientSingle . UnmeasuredHeader (SummedNutrient n p)

measured :: MeasuredNutrient -> NutTree -> NutrientChoice Node
measured h = NutrientSingle . MeasuredHeader h

unmeasured :: SummedNutrient -> NonEmpty (NutrientChoice Node) -> NutrientChoice Node
unmeasured h = NutrientSingle . UnmeasuredHeader h

nutTree :: SummedNutrient -> NonEmpty (NutrientChoice Node) -> NutTree
nutTree u xs =
  NutTree
    { ntFractions = xs
    , ntUnmeasuredHeader = u
    , ntUnmeasuredTree = Nothing
    }

measuredLeaves
  :: MeasuredNutrient
  -> SummedNutrient
  -> NonEmpty MeasuredNutrient
  -> NutrientChoice Node
measuredLeaves h u xs = measured h $ nutTree u (leaf <$> xs)

unmeasuredLeaves
  :: SummedNutrient
  -> NonEmpty MeasuredNutrient
  -> NutrientChoice Node
unmeasuredLeaves h xs = unmeasured h (leaf <$> xs)

groupLeaves :: Text -> Prefix -> NonEmpty MeasuredNutrient -> NutrientChoice Node
groupLeaves h u xs = group h u (leaf <$> xs)

unclassified :: Text -> Prefix -> SummedNutrient
unclassified x = SummedNutrient (T.append x " (unclassified)")

listDirect :: Prefix -> NonEmpty (NID, Text) -> NonEmpty MeasuredNutrient
listDirect p = fmap (uncurry go)
  where
    go i n = Direct $ DirectNutrient i n p
