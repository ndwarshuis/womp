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
import RIO
import qualified RIO.NonEmpty as N
import qualified RIO.Set as S
import qualified RIO.Text as T

standardMass :: Mass
standardMass = 100

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

-- | Lipid level
tfas :: MeasuredNutrient
tfas = Direct $ DirectNutrient 1257 "Trans Fatty Acids" Unity

mufas :: MeasuredNutrient
mufas = Direct $ DirectNutrient 1292 "Monounsaturated Fatty Acids" Unity

pufas :: MeasuredNutrient
pufas = Direct $ DirectNutrient 1293 "Polyunsaturated Fatty Acids" Unity

sfas :: MeasuredNutrient
sfas = Direct $ DirectNutrient 1258 "Saturated Fatty Acids" Unity

cholesterol :: MeasuredNutrient
cholesterol = Direct $ DirectNutrient 1253 "Cholesterol" Unity

phytosterols :: MeasuredNutrient
phytosterols = Direct $ DirectNutrient 1283 "Phytosterols" Unity

-- this doesn't strictly seem like a phytosterol but is still included in the
-- same section along with the rest (whatever)
stigmastadiene :: MeasuredNutrient
stigmastadiene = Direct $ DirectNutrient 2052 "Stigmastadiene" Micro

stigmasterol :: MeasuredNutrient
stigmasterol = Direct $ DirectNutrient 1285 "Stigmasterol" Micro

campesterol :: MeasuredNutrient
campesterol = Direct $ DirectNutrient 1286 "Campesterol" Micro

brassicasterol :: MeasuredNutrient
brassicasterol = Direct $ DirectNutrient 1287 "Brassicasterol" Micro

betaSitosterol :: MeasuredNutrient
betaSitosterol = Direct $ DirectNutrient 1288 "BetaSitosterol" Micro

campestanol :: MeasuredNutrient
campestanol = Direct $ DirectNutrient 1289 "Campestanol" Micro

betaSitostanol :: MeasuredNutrient
betaSitostanol = Direct $ DirectNutrient 1294 "BetaSitostanol" Micro

delta_5_avenasterol :: MeasuredNutrient
delta_5_avenasterol = Direct $ DirectNutrient 1296 "Delta5Avenasterol" Micro

delta_7_stigmastenol :: MeasuredNutrient
delta_7_stigmastenol = Direct $ DirectNutrient 2052 "Delta7Stigmastenol" Micro

ergosterol :: MeasuredNutrient
ergosterol = Direct $ DirectNutrient 1284 "Ergosterol" Micro

ergosta_7_enol :: MeasuredNutrient
ergosta_7_enol = Direct $ DirectNutrient 2060 "Ergosta-7-enol" Micro

ergosta_7_22_dienol :: MeasuredNutrient
ergosta_7_22_dienol = Direct $ DirectNutrient 2061 "Ergosta-7,22-dienol" Micro

ergosta_5_7_dienol :: MeasuredNutrient
ergosta_5_7_dienol = Direct $ DirectNutrient 2062 "Ergosta-5,7-dienol" Micro

-- TODO not exactly sure what this means, but hopefully it means "not the above"
-- otherPhytosterols :: MeasuredNutrient
-- otherPhytosterols = Direct $ DirectNutrient 1298 "Other Phytosterols" Unity

otherPhytosterols :: SummedNutrient
otherPhytosterols = SummedNutrient "Other Phytosterols" Unity

allSFAs :: NonEmpty MeasuredNutrient
allSFAs = fmap (uncurry go) ids
  where
    go carbons i = Direct $ DirectNutrient i (T.concat ["SFA ", tshow carbons, ":0"]) Unity
    ids =
      (4 :: Int, 1259)
        :| [ (5, 2003)
           , (6, 1260)
           , (7, 2004)
           , (8, 1261)
           , (9, 2005)
           , (10, 1262)
           , (11, 1335)
           , (12, 1263)
           , (13, 1332)
           , (14, 1264)
           , (15, 1299)
           , (16, 1265)
           , (17, 1300)
           , (18, 1266)
           , (20, 1267)
           , (21, 2006)
           , (22, 1273)
           , (23, 2007)
           , (24, 1301)
           ]

allTFAs :: NonEmpty MeasuredNutrient
allTFAs = fmap (uncurry go) ids
  where
    go (carbons, bonds) i = Direct $ DirectNutrient i (fmt carbons bonds) Unity
    fmt carbons bonds = T.concat ["TFA ", tshow carbons, ":", tshow bonds]
    ids =
      ((14 :: Int, 1 :: Int), 1281)
        :| [ ((16, 1), 1303)
           , ((17, 1), 2011)
           , ((18, 1), 1304)
           , ((18, 2), 1306)
           , ((18, 3), 2019)
           , ((20, 1), 2013)
           , ((22, 1), 1305)
           ]

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
    cis = FATree 1315 [(1412, "11t")] -- must be in important trans fat...

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
ufa uc i append = Direct $ DirectNutrient i (T.append n t) Unity
  where
    n = ufaName uc
    t = maybe "" (T.append " ") append

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
  Alternate $ AltNutrient "Total Sugars" Unity $ (1235, 1) :| [(1236, 1)]

sucrose :: MeasuredNutrient
sucrose = Direct $ DirectNutrient 1010 "Sucrose" Unity

glucose :: MeasuredNutrient
glucose = Direct $ DirectNutrient 1011 "Glucose" Unity

fructose :: MeasuredNutrient
fructose = Direct $ DirectNutrient 1012 "Fructose" Unity

lactose :: MeasuredNutrient
lactose = Direct $ DirectNutrient 1013 "Lactose" Unity

maltose :: MeasuredNutrient
maltose = Direct $ DirectNutrient 1014 "maltose" Unity

galactose :: MeasuredNutrient
galactose = Direct $ DirectNutrient 1075 "Galactose" Unity

raffinose :: MeasuredNutrient
raffinose = Direct $ DirectNutrient 1076 "Raffinose" Unity

stachyose :: MeasuredNutrient
stachyose = Direct $ DirectNutrient 1077 "Stachyose" Unity

verbascose :: MeasuredNutrient
verbascose = Direct $ DirectNutrient 2063 "Verbascose" Unity

-- TODO add inulin (1403)
-- TODO add lignin (1080)

tryptophan :: MeasuredNutrient
tryptophan = Direct $ DirectNutrient 1210 "Tryptophan" Milli

threonine :: MeasuredNutrient
threonine = Direct $ DirectNutrient 1211 "Threonine" Milli

isoleucine :: MeasuredNutrient
isoleucine = Direct $ DirectNutrient 1212 "Isoleucine" Milli

leucine :: MeasuredNutrient
leucine = Direct $ DirectNutrient 1213 "Leucine" Milli

lysine :: MeasuredNutrient
lysine = Direct $ DirectNutrient 1214 "Lysine" Milli

methionine :: MeasuredNutrient
methionine = Direct $ DirectNutrient 1215 "Methionine" Milli

cystine :: MeasuredNutrient
cystine = Direct $ DirectNutrient 1216 "Cystine" Milli

phenylalanine :: MeasuredNutrient
phenylalanine = Direct $ DirectNutrient 1217 "Phenylalanine" Milli

tyrosine :: MeasuredNutrient
tyrosine = Direct $ DirectNutrient 1218 "Tyrosine" Milli

valine :: MeasuredNutrient
valine = Direct $ DirectNutrient 1219 "Valine" Milli

arginine :: MeasuredNutrient
arginine = Direct $ DirectNutrient 1220 "Arginine" Milli

histidine :: MeasuredNutrient
histidine = Direct $ DirectNutrient 1221 "Histidine" Milli

alanine :: MeasuredNutrient
alanine = Direct $ DirectNutrient 1222 "Alanine" Milli

asparticAcid :: MeasuredNutrient
asparticAcid = Direct $ DirectNutrient 1223 "Aspartic Acid" Milli

glutamicAcid :: MeasuredNutrient
glutamicAcid = Direct $ DirectNutrient 1224 "Glutamic Acid" Milli

glycine :: MeasuredNutrient
glycine = Direct $ DirectNutrient 1225 "Glycine" Milli

proline :: MeasuredNutrient
proline = Direct $ DirectNutrient 1226 "Proline" Milli

serine :: MeasuredNutrient
serine = Direct $ DirectNutrient 1227 "Serine" Milli

hydroxyproline :: MeasuredNutrient
hydroxyproline = Direct $ DirectNutrient 1228 "Hydroxyproline" Milli

asparagine :: MeasuredNutrient
asparagine = Direct $ DirectNutrient 1231 "Asparagine" Milli

cysteine :: MeasuredNutrient
cysteine = Direct $ DirectNutrient 1232 "Cysteine" Milli

glutamine :: MeasuredNutrient
glutamine = Direct $ DirectNutrient 1233 "Glutamine" Milli

calcium :: MeasuredNutrient
calcium = Direct $ DirectNutrient 1087 "Calcium" Milli

iron :: MeasuredNutrient
iron = Direct $ DirectNutrient 1089 "Iron" Milli

magnesium :: MeasuredNutrient
magnesium = Direct $ DirectNutrient 1090 "Magnesium" Milli

phosphorus :: MeasuredNutrient
phosphorus = Direct $ DirectNutrient 1091 "Phosphorus" Milli

potassium :: MeasuredNutrient
potassium = Direct $ DirectNutrient 1092 "Potassium" Milli

sodium :: MeasuredNutrient
sodium =
  Alternate $
    AltNutrient "Sodium" Milli $
      (1093, 1) :| [(1149, saltConversion)]

sulfur :: MeasuredNutrient
sulfur = Direct $ DirectNutrient 1094 "Sulfur" Milli

zinc :: MeasuredNutrient
zinc = Direct $ DirectNutrient 1095 "Zinc" Milli

chromium :: MeasuredNutrient
chromium = Direct $ DirectNutrient 1096 "Chromium" Milli

cobalt :: MeasuredNutrient
cobalt = Direct $ DirectNutrient 1097 "Cobalt" Milli

copper :: MeasuredNutrient
copper = Direct $ DirectNutrient 1098 "Copper" Milli

iodine :: MeasuredNutrient
iodine = Direct $ DirectNutrient 1100 "Iodine" Milli

-- TODO since this actually says "chlorine" and not "chloride" I'm not sure if
-- this salt conversion really counts.
chlorine :: MeasuredNutrient
chlorine =
  Alternate $
    AltNutrient "Chlorine" Milli $
      -- convert NaCl to Cl-
      (1088, 1) :| [(1149, 1 - saltConversion)]

fluoride :: MeasuredNutrient
fluoride = Direct $ DirectNutrient 1099 "Fluoride" Milli

manganese :: MeasuredNutrient
manganese = Direct $ DirectNutrient 1101 "Manganese" Milli

molybdenum :: MeasuredNutrient
molybdenum = Direct $ DirectNutrient 1102 "Molybdenum" Milli

selenium :: MeasuredNutrient
selenium = Direct $ DirectNutrient 1103 "Selenium" Milli

boron :: MeasuredNutrient
boron = Direct $ DirectNutrient 1137 "Boron" Milli

nickel :: MeasuredNutrient
nickel = Direct $ DirectNutrient 1146 "Nickel" Milli

retinol :: MeasuredNutrient
retinol = Direct $ DirectNutrient 1105 "Retinol" Micro

alphaCarotene :: MeasuredNutrient
alphaCarotene = Direct $ DirectNutrient 1108 "alpha-carotene" Micro

betaCarotene :: MeasuredNutrient
betaCarotene = Direct $ DirectNutrient 1107 "beta-carotene" Micro

cisBetaCarotene :: MeasuredNutrient
cisBetaCarotene = Direct $ DirectNutrient 1159 "cis-beta-carotene" Micro

transBetaCarotene :: MeasuredNutrient
transBetaCarotene = Direct $ DirectNutrient 2028 "trans-beta-carotene" Micro

gammaCarotene :: MeasuredNutrient
gammaCarotene = Direct $ DirectNutrient 1118 "gamma-carotene" Micro

alphaCryptoxanthin :: MeasuredNutrient
alphaCryptoxanthin = Direct $ DirectNutrient 2032 "alpha-carotene" Micro

betaCryptoxanthin :: MeasuredNutrient
betaCryptoxanthin = Direct $ DirectNutrient 1120 "beta-carotene" Micro

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

-- TODO what to do with just Vitamin E (1124, 1158, 2068)?

-- TODO if one really wants to get nerdy we could weight these by affinity for
-- the vitamin E transport receptor (see wikipedia article)
tocopherolAlpha :: MeasuredNutrient
tocopherolAlpha = Direct $ DirectNutrient 1109 "Vitamin E (alpha-Tocopherol)" Micro

tocopherolBeta :: MeasuredNutrient
tocopherolBeta = Direct $ DirectNutrient 1125 "Vitamin E (beta-Tocopherol)" Micro

tocopherolGamma :: MeasuredNutrient
tocopherolGamma = Direct $ DirectNutrient 1126 "Vitamin E (gamma-Tocopherol)" Micro

tocopherolDelta :: MeasuredNutrient
tocopherolDelta = Direct $ DirectNutrient 1127 "Vitamin E (delta-Tocopherol)" Micro

tocotrienolAlpha :: MeasuredNutrient
tocotrienolAlpha = Direct $ DirectNutrient 1128 "Vitamin E (alpha-Tocotrienol)" Micro

tocotrienolBeta :: MeasuredNutrient
tocotrienolBeta = Direct $ DirectNutrient 1129 "Vitamin E (beta-Tocotrienol)" Micro

tocotrienolGamma :: MeasuredNutrient
tocotrienolGamma = Direct $ DirectNutrient 1130 "Vitamin E (gamma-Tocotrienol)" Micro

tocotrienolDelta :: MeasuredNutrient
tocotrienolDelta = Direct $ DirectNutrient 1131 "Vitamin E (delta-Tocotrienol)" Micro

-- NOTE this is (probably) different from "normal" vitamin E since synthetic
-- tocopherol is usually acetylated for stability, so technically this is a
-- different species
addedVitaminE :: MeasuredNutrient
addedVitaminE = Direct $ DirectNutrient 1242 "Vitamin E (synthetic)" Micro

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

freeCholine :: MeasuredNutrient
freeCholine = Direct $ DirectNutrient 1194 "Choline (unbound)" Milli

phosphoCholine :: MeasuredNutrient
phosphoCholine = Direct $ DirectNutrient 1195 "Choline (phosphocholine)" Milli

phosphotidylCholine :: MeasuredNutrient
phosphotidylCholine = Direct $ DirectNutrient 1196 "Choline (phosphotidyl-choline)" Milli

glycerophosphoCholine :: MeasuredNutrient
glycerophosphoCholine = Direct $ DirectNutrient 1197 "Choline (glycerophospho-choline)" Milli

sphingomyelinCholine :: MeasuredNutrient
sphingomyelinCholine = Direct $ DirectNutrient 1199 "Choline (sphingomyelin)" Milli

-- | The phytoestrogens every bro who eats soy is worried about
isoflavones :: SummedNutrient
isoflavones = SummedNutrient "Isoflavones" Milli

daidzein :: MeasuredNutrient
daidzein = Direct $ DirectNutrient 1340 "Daidzein" Milli

daidzin :: MeasuredNutrient
daidzin = Direct $ DirectNutrient 2049 "Daidzin" Milli

genistein :: MeasuredNutrient
genistein = Direct $ DirectNutrient 1341 "Genistein" Milli

genistin :: MeasuredNutrient
genistin = Direct $ DirectNutrient 2050 "Genistin" Milli

glycitin :: MeasuredNutrient
glycitin = Direct $ DirectNutrient 2051 "Glycitin" Milli

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

otherSugars :: SummedNutrient
otherSugars = SummedNutrient "Other Sugars" Unity

otherSugarAlcohols :: SummedNutrient
otherSugarAlcohols = SummedNutrient "Other Sugar Alcohols" Unity

otherSFAs :: SummedNutrient
otherSFAs = SummedNutrient "Other SFAs" Unity

otherTFAs :: SummedNutrient
otherTFAs = SummedNutrient "Other TFAs" Unity

otherMUFAs :: SummedNutrient
otherMUFAs = SummedNutrient "Other MUFAs" Unity

otherPUFAs :: SummedNutrient
otherPUFAs = SummedNutrient "Other PUFAs" Unity

otherLipids :: SummedNutrient
otherLipids = SummedNutrient "Other Lipids" Unity

otherProteinMass :: SummedNutrient
otherProteinMass = SummedNutrient "Other Protein Mass" Milli

otherCarbs :: SummedNutrient
otherCarbs = SummedNutrient "Other Carbs" Unity

otherFiberBySolubility :: SummedNutrient
otherFiberBySolubility = SummedNutrient "Other Fiber (unclassified solubility)" Unity

otherFiberByWeight :: SummedNutrient
otherFiberByWeight = SummedNutrient "Other Fiber (unclassified weight)" Unity

otherInorganics :: SummedNutrient
otherInorganics = SummedNutrient "Other Inorganics" Unity

-- pufa_18_2_other :: SummedNutrient
-- pufa_18_2_other = SummedNutrient "PUFA 18:2 (unclassified)" Unity

-- pufa_18_3_other :: SummedNutrient
-- pufa_18_3_other = SummedNutrient "PUFA 18:3 (unclassified)" Unity

-- pufa_20_3_other :: SummedNutrient
-- pufa_20_3_other = SummedNutrient "PUFA 20:3 (unclassified)" Unity

-- mufa_22_1_other :: SummedNutrient
-- mufa_22_1_other = SummedNutrient "MUFA 22:1 (unclassified)" Unity

otherCholine :: SummedNutrient
otherCholine = SummedNutrient "Choline (unclassified)" Milli

-- otherFolate :: SummedNutrient
-- otherFolate = SummedNutrient "Folates (unclassified)" Milli

otherVitaminD :: SummedNutrient
otherVitaminD = SummedNutrient "Vitamin D (unclassified)" Micro

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
    :| [ fluoride
       , sodium
       , magnesium
       , phosphorus
       , sulfur
       , chlorine
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
       , addedVitaminE
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
                      :| [ measuredLeaves tfas otherTFAs allTFAs
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

-- measuredLeaves
--   pufa_18_2
--   pufa_18_2_other
--   (pufa_18_2_CLA :| [pufa_18_2_n6_cc])
--   :| [ measuredLeaves
--         pufa_18_3
--         pufa_18_3_other
--         (pufa_18_3_n3_ccc :| [pufa_18_3_n6_ccc, pufa_18_3i])
--      , leaf pufa_18_4
--      , unmeasuredLeaves pufa_20_2 (pufa_20_2_n6_cc :| [])
--      , measuredLeaves
--         pufa_20_3
--         pufa_20_3_other
--         (pufa_20_3_n3 :| [pufa_20_3_n6, pufa_20_3_n9])
--      , leaf pufa_20_4
--      , measuredLeaves pufa_20_5 undefined (pufa_20_5_n3 :| [])
--      , leaf pufa_22_2
--      , leaf pufa_22_3
--      , leaf pufa_22_4
--      , measuredLeaves pufa_22_5 undefined (pufa_22_5_n3 :| [])
--      , measuredLeaves pufa_22_6 undefined (pufa_22_6_n3 :| [])
--      ]

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

-- | The fraction of NaCl which is Na+
saltConversion :: RealFrac a => a
saltConversion = 22.990 / (22.990 + 35.45)
