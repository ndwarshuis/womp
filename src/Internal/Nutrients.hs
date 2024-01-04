module Internal.Nutrients where

import Data.Scientific
import Internal.Types.Main
import RIO

standardMass :: Scientific
standardMass = 100

water :: MeasuredNutrient
water = Direct $ DirectNutrient 1051 "Water" Unity

protein :: Scientific -> MeasuredNutrient
protein n2Factor =
  Alternate $
    AltNutrient "Protein" Unity $
      (proteinId, Nothing) :| [(nitrogenId, Just n2Factor)]
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

phytosterols :: SummedNutrient
phytosterols = SummedNutrient "Phytosterols" Unity

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
otherPhytosterols :: MeasuredNutrient
otherPhytosterols = Direct $ DirectNutrient 1298 "Other Phytosterols" Unity

sfa_4_0 :: MeasuredNutrient
sfa_4_0 = Direct $ DirectNutrient 1259 "SFA 4:0" Unity

sfa_5_0 :: MeasuredNutrient
sfa_5_0 = Direct $ DirectNutrient 2003 "SFA 5:0" Unity

sfa_6_0 :: MeasuredNutrient
sfa_6_0 = Direct $ DirectNutrient 1260 "SFA 6:0" Unity

sfa_7_0 :: MeasuredNutrient
sfa_7_0 = Direct $ DirectNutrient 2004 "SFA 7:0" Unity

sfa_8_0 :: MeasuredNutrient
sfa_8_0 = Direct $ DirectNutrient 1261 "SFA 8:0" Unity

sfa_9_0 :: MeasuredNutrient
sfa_9_0 = Direct $ DirectNutrient 2005 "SFA 9:0" Unity

sfa_10_0 :: MeasuredNutrient
sfa_10_0 = Direct $ DirectNutrient 1262 "SFA 10:0" Unity

sfa_11_0 :: MeasuredNutrient
sfa_11_0 = Direct $ DirectNutrient 1335 "SFA 11:0" Unity

sfa_12_0 :: MeasuredNutrient
sfa_12_0 = Direct $ DirectNutrient 1263 "SFA 12:0" Unity

sfa_14_0 :: MeasuredNutrient
sfa_14_0 = Direct $ DirectNutrient 1264 "SFA 14:0" Unity

sfa_15_0 :: MeasuredNutrient
sfa_15_0 = Direct $ DirectNutrient 1299 "SFA 15:0" Unity

sfa_16_0 :: MeasuredNutrient
sfa_16_0 = Direct $ DirectNutrient 1265 "SFA 16:0" Unity

sfa_17_0 :: MeasuredNutrient
sfa_17_0 = Direct $ DirectNutrient 1300 "SFA 17:0" Unity

sfa_18_0 :: MeasuredNutrient
sfa_18_0 = Direct $ DirectNutrient 1266 "SFA 18:0" Unity

sfa_20_0 :: MeasuredNutrient
sfa_20_0 = Direct $ DirectNutrient 1267 "SFA 20:0" Unity

sfa_21_0 :: MeasuredNutrient
sfa_21_0 = Direct $ DirectNutrient 2006 "SFA 21:0" Unity

sfa_22_0 :: MeasuredNutrient
sfa_22_0 = Direct $ DirectNutrient 1273 "SFA 22:0" Unity

sfa_23_0 :: MeasuredNutrient
sfa_23_0 = Direct $ DirectNutrient 2007 "SFA 23:0" Unity

sfa_24_0 :: MeasuredNutrient
sfa_24_0 = Direct $ DirectNutrient 1301 "SFA 24:0" Unity

mufa_12_1 :: MeasuredNutrient
mufa_12_1 = Direct $ DirectNutrient 2008 "MUFA 12:1" Unity

-- NOTE: it seems most of the MUFAs are not characterized further other than
-- number of carbons. In this case, several of the MUFAs are redundant (ie the
-- one's that end in c and the ones that don't). Rather than have multiple
-- levels for each, collapse them into single layer to keep my code less lame

-- TODO not sure how this one works, it is almost always analytical but
-- sometimes summed in which case I have no idea what the inputs are because
-- there are no other 14C MUFAs in the db)
mufa_14_1 :: MeasuredNutrient
mufa_14_1 = Direct $ DirectNutrient 2009 "MUFA 14:1" Unity

mufa_15_1 :: MeasuredNutrient
mufa_15_1 = Direct $ DirectNutrient 1333 "MUFA 15:1" Unity

mufa_16_1 :: MeasuredNutrient
mufa_16_1 = Direct $ DirectNutrient 1314 "MUFA 16:1" Unity

mufa_17_1 :: MeasuredNutrient
mufa_17_1 = Direct $ DirectNutrient 1323 "MUFA 17:1" Unity

mufa_18_1 :: MeasuredNutrient
mufa_18_1 = Alternate $ AltNutrient "MUFA 18:1" Unity $ (1315, Nothing) :| [(1268, Nothing)]

mufa_20_1 :: MeasuredNutrient
mufa_20_1 = Alternate $ AltNutrient "MUFA 20:1" Unity $ (2012, Nothing) :| [(1277, Nothing)]

mufa_22_1 :: MeasuredNutrient
mufa_22_1 = Alternate $ AltNutrient "MUFA 22:1" Unity $ (1317, Nothing) :| [(2012, Nothing)]

mufa_22_1_n9 :: MeasuredNutrient
mufa_22_1_n9 = Direct $ DirectNutrient 2014 "MUFA 22:1 ω-9 (Erucic Acid)" Unity

mufa_22_1_n11 :: MeasuredNutrient
mufa_22_1_n11 = Direct $ DirectNutrient 2015 "MUFA 22:1 ω-11" Unity

mufa_24_1 :: MeasuredNutrient
mufa_24_1 = Direct $ DirectNutrient 1312 "MUFA 24:1" Unity

tfa_14_1 :: MeasuredNutrient
tfa_14_1 = Direct $ DirectNutrient 1281 "TFA 14:1" Unity

tfa_16_1 :: MeasuredNutrient
tfa_16_1 = Direct $ DirectNutrient 1303 "TFA 16:1" Unity

tfa_17_1 :: MeasuredNutrient
tfa_17_1 = Direct $ DirectNutrient 2011 "TFA 17:1" Unity

tfa_18_1 :: MeasuredNutrient
tfa_18_1 = Direct $ DirectNutrient 1304 "TFA 18:1" Unity

tfa_18_2 :: MeasuredNutrient
tfa_18_2 = Direct $ DirectNutrient 1306 "TFA 18:2" Unity

tfa_18_3 :: MeasuredNutrient
tfa_18_3 = Direct $ DirectNutrient 2019 "TFA 18:3" Unity

tfa_20_1 :: MeasuredNutrient
tfa_20_1 = Direct $ DirectNutrient 2013 "TFA 20:1" Unity

tfa_22_1 :: MeasuredNutrient
tfa_22_1 = Direct $ DirectNutrient 1305 "TFA 22:1" Unity

pufa_18_2 :: MeasuredNutrient
pufa_18_2 = Direct $ DirectNutrient 1269 "PUFA 18:2" Unity

pufa_18_2_CLA :: MeasuredNutrient
pufa_18_2_CLA = Direct $ DirectNutrient 1311 "PUFA 18:2 (conjugated linoleic acids)" Unity

pufa_18_2_n6_cc :: MeasuredNutrient
pufa_18_2_n6_cc = Direct $ DirectNutrient 1316 "PUFA 18:2 ω-6 c,c (Linoleic Acid)" Unity

pufa_18_3 :: MeasuredNutrient
pufa_18_3 = Direct $ DirectNutrient 1270 "PUFA 18:3" Unity

-- at least I think this is what "i" means
pufa_18_3i :: MeasuredNutrient
pufa_18_3i = Direct $ DirectNutrient 1409 "PUFA 18:2 isomers" Unity

pufa_18_3_n6_ccc :: MeasuredNutrient
pufa_18_3_n6_ccc = Direct $ DirectNutrient 1321 "PUFA 18:3 ω-6 c,c,c (Gamma-linolenic Acid)" Unity

pufa_18_3_n3_ccc :: MeasuredNutrient
pufa_18_3_n3_ccc = Direct $ DirectNutrient 1404 "PUFA 18:3 ω-3 c,c,c (Alpha-linolenic Acid)" Unity

pufa_18_4 :: MeasuredNutrient
pufa_18_4 = Direct $ DirectNutrient 1276 "PUFA 18:4" Unity

pufa_20_2 :: SummedNutrient
pufa_20_2 = SummedNutrient "PUFA 20:2" Unity

pufa_20_2_n6_cc :: MeasuredNutrient
pufa_20_2_n6_cc = Direct $ DirectNutrient 1313 "PUFA 20:2_n6_cc" Unity

pufa_20_3 :: MeasuredNutrient
pufa_20_3 = Direct $ DirectNutrient 1325 "PUFA 20:3" Unity

pufa_20_3_n3 :: MeasuredNutrient
pufa_20_3_n3 = Direct $ DirectNutrient 1405 "PUFA 20:3 ω-3 c,c,c (Eicosatetraenoic Acid)" Unity

pufa_20_3_n6 :: MeasuredNutrient
pufa_20_3_n6 = Direct $ DirectNutrient 1406 "PUFA 20:3 ω-6 c,c,c (Dihomo-gamma-linolenic Acid)" Unity

pufa_20_3_n9 :: MeasuredNutrient
pufa_20_3_n9 = Direct $ DirectNutrient 1414 "PUFA 20:3 ω-9 c,c,c (Mead Acid)" Unity

pufa_20_4 :: MeasuredNutrient
pufa_20_4 = Direct $ DirectNutrient 1271 "PUFA 20:4" Unity

pufa_20_5 :: SummedNutrient
pufa_20_5 = SummedNutrient "PUFA 20:5" Unity

pufa_20_5_n3 :: MeasuredNutrient
pufa_20_5_n3 = Direct $ DirectNutrient 1278 "PUFA 20:5_n3" Unity

pufa_22_2 :: MeasuredNutrient
pufa_22_2 = Direct $ DirectNutrient 1334 "PUFA 22:2" Unity

pufa_22_3 :: MeasuredNutrient
pufa_22_3 = Direct $ DirectNutrient 2021 "PUFA 22:3" Unity

pufa_22_4 :: MeasuredNutrient
pufa_22_4 = Direct $ DirectNutrient 1411 "PUFA 22:4" Unity

pufa_22_5 :: SummedNutrient
pufa_22_5 = SummedNutrient "PUFA 22:5" Unity

pufa_22_5_n3 :: MeasuredNutrient
pufa_22_5_n3 = Direct $ DirectNutrient 1280 "PUFA 22:5 ω-3 c,c,c,c,c (Docosapentaenoic Acid)" Unity

pufa_22_6 :: SummedNutrient
pufa_22_6 = SummedNutrient "PUFA 22:5" Unity

pufa_22_6_n3 :: MeasuredNutrient
pufa_22_6_n3 = Direct $ DirectNutrient 1272 "PUFA 22:6 ω-3 c,c,c,c,c,c (Docosahexaenoic Acid)" Unity

-- | Carbohydrate level
betaGlucan :: MeasuredNutrient
betaGlucan = Direct $ DirectNutrient 2058 "Beta Glucans" Unity

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

-- | Sugar level
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
sodium = Direct $ DirectNutrient 1093 "Sodium" Milli

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
alphaCarotene = Direct $ DirectNutrient 1108 "α-carotene" Micro

betaCarotene :: MeasuredNutrient
betaCarotene = Direct $ DirectNutrient 1107 "β-carotene" Micro

cisBetaCarotene :: MeasuredNutrient
cisBetaCarotene = Direct $ DirectNutrient 1159 "cis-β-carotene" Micro

transBetaCarotene :: MeasuredNutrient
transBetaCarotene = Direct $ DirectNutrient 2028 "trans-β-carotene" Micro

gammaCarotene :: MeasuredNutrient
gammaCarotene = Direct $ DirectNutrient 1118 "γ-carotene" Micro

alphaCryptoxanthin :: MeasuredNutrient
alphaCryptoxanthin = Direct $ DirectNutrient 2032 "α-carotene" Micro

betaCryptoxanthin :: MeasuredNutrient
betaCryptoxanthin = Direct $ DirectNutrient 1120 "β-carotene" Micro

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

vitaminB9 :: MeasuredNutrient
vitaminB9 = Direct $ DirectNutrient 1177 "Vitamin B9 (total folate)" Micro

folinicAcid :: MeasuredNutrient
folinicAcid = Direct $ DirectNutrient 1192 "5-Formyl Tetrahydrofolic acid" Micro

levomefolicAcid :: MeasuredNutrient
levomefolicAcid = Direct $ DirectNutrient 1188 "5-Methyl Tetrahydrofolate" Micro

-- There is also this thing called "10-Formyl folic acid (10HCOFA)" which does
-- not appear to be a real thing. It was measured according to the same method
-- as the other two folate species (according to
-- doi.org/10.1016/j.foodchem.2004.08.007) although the cited method only
-- alludes to levomefolic acid.

vitaminB12 :: MeasuredNutrient
vitaminB12 = Direct $ DirectNutrient 1178 "Vitamin B12 (cobalamins)" Micro

vitaminC :: MeasuredNutrient
vitaminC = Direct $ DirectNutrient 1162 "Vitamin C (ascorbic acid)" Milli

vitaminD :: SummedNutrient
vitaminD = SummedNutrient "Vitamin D (total)" Micro

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

-- TODO if one really wants to get nerdy we could weight these by affinity for
-- the vitamin E transport receptor (see wikipedia article)
tocopherolAlpha :: MeasuredNutrient
tocopherolAlpha = Direct $ DirectNutrient 1109 "Vitamin E (α-Tocopherol)" Micro

tocopherolBeta :: MeasuredNutrient
tocopherolBeta = Direct $ DirectNutrient 1125 "Vitamin E (β-Tocopherol)" Micro

tocopherolGamma :: MeasuredNutrient
tocopherolGamma = Direct $ DirectNutrient 1126 "Vitamin E (γ-Tocopherol)" Micro

tocopherolDelta :: MeasuredNutrient
tocopherolDelta = Direct $ DirectNutrient 1127 "Vitamin E (δ-Tocopherol)" Micro

tocotrienolAlpha :: MeasuredNutrient
tocotrienolAlpha = Direct $ DirectNutrient 1128 "Vitamin E (α-Tocotrienol)" Micro

tocotrienolBeta :: MeasuredNutrient
tocotrienolBeta = Direct $ DirectNutrient 1129 "Vitamin E (β-Tocotrienol)" Micro

tocotrienolGamma :: MeasuredNutrient
tocotrienolGamma = Direct $ DirectNutrient 1130 "Vitamin E (γ-Tocotrienol)" Micro

tocotrienolDelta :: MeasuredNutrient
tocotrienolDelta = Direct $ DirectNutrient 1131 "Vitamin E (δ-Tocotrienol)" Micro

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
transLycopene = Direct $ DirectNutrient 2028 "trans-Lycopene" Micro

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

citricAcid :: MeasuredNutrient
citricAcid = Direct $ DirectNutrient 1032 "Citric Acid" Milli

malicAcid :: MeasuredNutrient
malicAcid = Direct $ DirectNutrient 1039 "Malic Acid" Milli

oxalicAcid :: MeasuredNutrient
oxalicAcid = Direct $ DirectNutrient 1041 "Oxalic Acid" Milli

pyruvicAcid :: MeasuredNutrient
pyruvicAcid = Direct $ DirectNutrient 1043 "Pyruvic Acid" Milli

quinicAcid :: MeasuredNutrient
quinicAcid = Direct $ DirectNutrient 1044 "Quinic Acid" Milli

taurine :: MeasuredNutrient
taurine = Direct $ DirectNutrient 1234 "Taurine" Milli

ergothioneine :: MeasuredNutrient
ergothioneine = Direct $ DirectNutrient 2057 "Ergothioneine" Milli

phytoene :: MeasuredNutrient
phytoene = Direct $ DirectNutrient 1116 "Phytoene" Milli

phytofluene :: MeasuredNutrient
phytofluene = Direct $ DirectNutrient 1117 "Phytofluene" Milli

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

totalSugars :: SummedNutrient
totalSugars = SummedNutrient "Total Sugars" Unity

pufa_18_2_other :: SummedNutrient
pufa_18_2_other = SummedNutrient "PUFA 18:2 (unclassified)" Unity

pufa_18_3_other :: SummedNutrient
pufa_18_3_other = SummedNutrient "PUFA 18:3 (unclassified)" Unity

pufa_20_3_other :: SummedNutrient
pufa_20_3_other = SummedNutrient "PUFA 20:3 (unclassified)" Unity

mufa_22_1_other :: SummedNutrient
mufa_22_1_other = SummedNutrient "MUFA 22:1 (unclassified)" Unity

otherCholine :: SummedNutrient
otherCholine = SummedNutrient "Choline (unclassified)" Milli

otherFolate :: SummedNutrient
otherFolate = SummedNutrient "Folates (unclassified)" Milli
