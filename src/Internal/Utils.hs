module Internal.Utils where

import Control.Monad.Error.Class
import Data.Scientific
import Data.Semigroup
import GHC.Conc (getNumProcessors)
import Internal.Types.Dhall
import Internal.Types.FoodItem
import Internal.Types.Main
import RIO
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as N
import qualified RIO.Text as T
import RIO.Time
import UnliftIO.Concurrent
import UnliftIO.Directory

--------------------------------------------------------------------------------
-- error handling

throwAppError :: MonadAppError m => AppError -> m a
throwAppError e = throwError $ AppException [e]

throwAppErrorIO :: MonadUnliftIO m => AppError -> m a
throwAppErrorIO = fromEither . throwAppError

-- combineError3
--   :: (Semigroup e, MonadError e m)
--   => m a
--   -> m b
--   -> m c
--   -> (a -> b -> c -> d)
--   -> m d
-- combineError3 a b c f =
--   combineError (combineError a b (,)) c $ \(x, y) z -> f x y z

-- combineError
--   :: (Semigroup e, MonadError e m)
--   => m a
--   -> m b
--   -> (a -> b -> c)
--   -> m c
-- combineError a b f = combineErrorM a b (\x y -> pure $ f x y)

-- combineErrorM
--   :: (Semigroup e, MonadError e m)
--   => m a
--   -> m b
--   -> (a -> b -> m c)
--   -> m c
-- combineErrorM a b f = do
--   a' <- catchError a $ \e ->
--     throwError =<< catchError (e <$ b) (return . (e <>))
--   f a' =<< b

-- mapErrors :: (Traversable t, MonadAppError m) => (a -> m b) -> t a -> m (t b)
-- -- First, record number of each action. Then try each action. On first failure,
-- -- note it's position in the sequence, skip ahead to the untried actions,
-- -- collect failures and add to the first failure.
-- mapErrors f xs = mapM go $ enumTraversable xs
--   where
--     go (n, x) = catchError (f x) $ \e -> do
--       es <- fmap catMaybes $ mapM (err . f) $ drop (n + 1) $ toList xs
--       throwError $ foldr (<>) e es
--     err x = catchError (Nothing <$ x) (pure . Just)

combineErrorIO2 :: MonadUnliftIO m => m a -> m b -> (a -> b -> c) -> m c
combineErrorIO2 a b f = combineErrorIOM2 a b (\x y -> pure $ f x y)

combineErrorIO3 :: MonadUnliftIO m => m a -> m b -> m c -> (a -> b -> c -> d) -> m d
combineErrorIO3 a b c f = combineErrorIOM3 a b c (\x y z -> pure $ f x y z)

combineErrorIOM2 :: MonadUnliftIO m => m a -> m b -> (a -> b -> m c) -> m c
combineErrorIOM2 a b f = do
  a' <- catch a $ \(AppException es) ->
    (throwIO . AppException)
      =<< catch (es <$ b) (\(AppException es') -> return (es' ++ es))
  f a' =<< b

combineErrorIOM3 :: MonadUnliftIO m => m a -> m b -> m c -> (a -> b -> c -> m d) -> m d
combineErrorIOM3 a b c f =
  combineErrorIOM2 (combineErrorIOM2 a b (curry return)) c $ \(x, y) z -> f x y z

enumTraversable :: (Num n, Traversable t) => t a -> t (n, a)
enumTraversable = snd . L.mapAccumL go 0
  where
    go n x = (n + 1, (n, x))

mapErrorsIO :: (Traversable t, MonadUnliftIO m) => (a -> m b) -> t a -> m (t b)
mapErrorsIO f xs = mapM go $ enumTraversable xs
  where
    go (n, x) = catch (f x) $ \(AppException e) -> do
      es <- fmap catMaybes $ mapM (err . f) $ drop (n + 1) $ toList xs
      throwIO $ AppException $ foldr (<>) e es
    err x = catch (Nothing <$ x) $ \(AppException es) -> pure $ Just es

-- TODO not dry
mapPooledErrorsIO :: (Traversable t, MonadUnliftIO m) => (a -> m b) -> t a -> m (t b)
mapPooledErrorsIO f xs = pooledMapConcurrently go $ enumTraversable xs
  where
    go (n, x) = catch (f x) $ \(AppException e) -> do
      es <- fmap catMaybes $ mapM (err . f) $ drop (n + 1) $ toList xs
      throwIO $ AppException $ foldr (<>) e es
    err x = catch (Nothing <$ x) $ \(AppException es) -> pure $ Just es

showError :: AppError -> [Text]
showError other = case other of
  PrefixError p -> [T.append "Could not parse to prefix: " p]
  NormalizeError x -> [T.append "Normalization constant must be 1 or more, got " $ tshow x]
  EmptySchedule True -> ["Empty schedule after expanding date ranges"]
  EmptySchedule False -> ["Schedule in plan is empty"]
  MassError s m -> [T.concat ["negative mass ", tshow m, " for ingredient ", tshow s]]
  DateDaysEndError x -> [T.append "--end must be 1 or more, got " $ tshow x]
  IntervalError x -> [T.append "--interval must be 1 or more, got " $ tshow x]
  SortKeys s -> [T.append "unable to parse sort order: " s]
  FilterKeys f -> [T.append "unable to parse filter keys: " f]
  MissingCustom c -> [T.append "Custom ingredient not found: " c]
  CustomIngError c -> [fmtCustomError c]
  FileTypeError f -> [T.append "File must be .yml/yaml or .dhall: " $ T.pack f]
  JSONError e -> [T.append "JSON parse error: " $ decodeUtf8Lenient e]
  EmptyMeal n -> [T.append "Meal has no ingredients: " n]
  MissingAPIKey p -> [T.append "Could not read API key from path: " $ T.pack p]
  DaySpanError -> ["end date must be at least one day after start date"]
  DatePatternError s b r p -> [T.unwords [msg, "in pattern: ", pat]]
    where
      pat =
        keyVals $
          [ (k, v)
          | (k, Just v) <-
              [ ("start", Just $ tshow s)
              , ("by", Just $ tshow b)
              , ("repeats", tshow <$> r)
              ]
          ]
      msg = case p of
        ZeroLength -> "Zero repeat length"
        ZeroRepeats -> "Zero repeats"

fmtCustomError :: CustomIngError -> Text
fmtCustomError (CustomDups n ns) =
  T.concat
    [ "Duplicate nutrient IDs found for '"
    , n
    , "': "
    , T.intercalate ", " $ tshow <$> N.toList ns
    ]
fmtCustomError (TooMuchMass n) = T.append "Mass for custom ingredient over 100g: " n

keyVals :: [(Text, Text)] -> Text
keyVals = T.intercalate "; " . fmap (uncurry keyVal)

keyVal :: Text -> Text -> Text
keyVal a b = T.concat [a, "=", b]

--------------------------------------------------------------------------------
-- math

prefixValue :: Prefix -> Int
prefixValue Nano = -9
prefixValue Micro = -6
prefixValue Milli = -3
prefixValue Centi = -2
prefixValue Deci = -1
prefixValue Unity = 0
prefixValue Deca = 1
prefixValue Hecto = 2
prefixValue Kilo = 3
prefixValue Mega = 6
prefixValue Giga = 9

parseUnit :: Text -> Maybe Unit
parseUnit s = catchError nonUnity (const def)
  where
    def = parseName Unity s
    nonUnity = case T.splitAt 1 s of
      ("G", rest) -> parseName Giga rest
      ("M", rest) -> parseName Mega rest
      ("k", rest) -> parseName Kilo rest
      ("h", rest) -> parseName Hecto rest
      ("d", rest) -> parseName Deci rest
      ("c", rest) -> parseName Centi rest
      ("m", rest) -> parseName Milli rest
      ("µ", rest) -> parseName Micro rest
      ("n", rest) -> parseName Nano rest
      _ -> case T.splitAt 2 s of
        ("da", rest) -> parseName Deca rest
        _ -> def
    parseName p r = case r of
      "cal" -> return $ Unit p Calorie
      "g" -> return $ Unit p Gram
      _ -> Nothing

parseCLIPrefix :: Text -> Maybe Prefix
parseCLIPrefix "G" = Just Giga
parseCLIPrefix "M" = Just Mega
parseCLIPrefix "K" = Just Kilo
parseCLIPrefix "h" = Just Hecto
parseCLIPrefix "da" = Just Deca
parseCLIPrefix "-" = Just Unity
parseCLIPrefix "d" = Just Deci
parseCLIPrefix "c" = Just Centi
parseCLIPrefix "m" = Just Milli
parseCLIPrefix "u" = Just Micro
parseCLIPrefix "n" = Just Nano
parseCLIPrefix s = readMaybe $ T.unpack $ T.toTitle s

raisePower :: Int -> Scientific -> Scientific
raisePower x s = scientific (coefficient s) (base10Exponent s + x)

roundDigits :: forall a. RealFrac a => Int -> a -> a
roundDigits p = (/ d) . go . round . (* d)
  where
    d = 10 ^ p
    go :: Int -> a
    go = fromIntegral

toUnity :: Prefix -> Scientific -> Scientific
toUnity p = raisePower (prefixValue p)

autoPrefix :: (Ord a, Fractional a) => a -> Prefix
autoPrefix s =
  maybe maxBound fst $
    L.find ((abs s <) . snd) $
      fmap
        (\p -> (p,) $ (10 ^^) $ (+ 3) $ prefixValue p)
        -- NOTE only use multiples of three for display
        [Nano, Micro, Milli, Unity, Kilo, Mega, Giga]

--------------------------------------------------------------------------------
-- date pattern expansion

checkCronPat :: MonadAppError m => Cron -> m ()
checkCronPat Cron {cronYear, cronMonth, cronDay} =
  mapM_ (mapM go) [cronYear, cronMonth, cronDay]
  where
    go (Repeat p) = checkRepeatPat p
    go _ = return ()

checkRepeatPat :: MonadAppError m => RepeatPat -> m ()
checkRepeatPat RepeatPat {rpStart = s, rpBy = b, rpRepeats = r}
  | b == 0 = throwAppError $ DatePatternError s b r ZeroLength
  | r == Just 0 = throwAppError $ DatePatternError s b r ZeroRepeats
  | otherwise = return ()

expandCronPat :: DaySpan -> Cron -> [Day]
expandCronPat b Cron {cronYear, cronMonth, cronDay, cronWeekly} =
  filter validWeekday $
    mapMaybe (uncurry3 toDay) $
      takeWhile (\((y, _), m, d) -> (y, m, d) <= (yb1, mb1, db1)) $
        dropWhile (\((y, _), m, d) -> (y, m, d) < (yb0, mb0, db0)) $
          [(y, m, d) | y <- (\y -> (y, isLeapYear y)) <$> ys, m <- ms, d <- ds]
  where
    ys = case cronYear of
      Nothing -> [yb0 .. yb1]
      Just pat ->
        dropWhile (< yb0) $
          fromIntegral
            <$> expandMDYPat (fromIntegral yb0) (fromIntegral yb1) pat
    ms = expandMD 12 cronMonth
    ds = expandMD 31 cronDay
    (s, e) = fromDaySpan b
    (yb0, mb0, db0) = toGregorian s
    (yb1, mb1, db1) = toGregorian $ addDays (-1) e
    expandMD lim =
      (fromIntegral <$>)
        . maybe [1 .. lim] (expandMDYPat 1 lim)
    expandW (OnDay x) = [fromEnum x]
    expandW (OnDays xs) = fromEnum <$> xs
    ws = maybe [] expandW cronWeekly
    validWeekday = if null ws then const True else \day -> dayToWeekday day `elem` ws
    toDay (y, leap) m d
      | m == 2 && (not leap && d > 28 || leap && d > 29) = Nothing
      | m `elem` [4, 6, 9, 11] && d > 30 = Nothing
      | otherwise = Just $ fromGregorian y m d

-- ASSUME that the repeat pattern is valid
expandMDYPat :: Natural -> Natural -> MDYPat -> [Natural]
expandMDYPat lower upper (Single x) = [x | lower <= x && x <= upper]
expandMDYPat lower upper (Multi xs) = dropWhile (<= lower) $ takeWhile (<= upper) xs
expandMDYPat lower upper (After x) = [max lower x .. upper]
expandMDYPat lower upper (Before x) = [lower .. min upper x]
expandMDYPat lower upper (Between (Btw btStart btEnd)) =
  [max lower btStart .. min upper btEnd]
expandMDYPat lower upper (Repeat RepeatPat {rpStart = s, rpBy = b, rpRepeats = r}) =
  -- ASSUME b and (Just r) > 0
  let k = maybe upper (\n -> min (s + b * (n - 1)) upper) r
   in dropWhile (<= lower) $ takeWhile (<= k) [s + i * b | i <- [0 ..]]

dayToWeekday :: Day -> Int
dayToWeekday (ModifiedJulianDay d) = mod (fromIntegral d + 3) 7

--------------------------------------------------------------------------------
-- nonempty

append :: NonEmpty a -> [a] -> NonEmpty a
append (x :| xs) ys = x :| xs ++ ys

take1 :: Int -> NonEmpty a -> NonEmpty a
take1 n (x :| xs) = x :| take (n - 1) xs

groupByTup :: Eq a => NonEmpty (a, b) -> NonEmpty (a, NonEmpty b)
groupByTup = fmap (\xs -> (fst $ N.head xs, snd <$> xs)) . N.groupWith1 fst

flatten :: NonEmpty (a, NonEmpty b) -> NonEmpty (a, b)
flatten = sconcat . fmap go
  where
    go (x, ys) = (x,) <$> ys

nonEmptyProduct :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
nonEmptyProduct f xs = sconcat . fmap (\y -> fmap (`f` y) xs)

--------------------------------------------------------------------------------
-- misc

withNonEmpty :: (Eq a, Monoid a) => (a -> b) -> b -> a -> b
withNonEmpty f d x
  | mempty == x = d
  | otherwise = f x

-- TODO could make this more general...if I feel like it
findDups :: Ord a => [a] -> [a]
findDups = fmap N.head . filter ((> 1) . length) . N.group . L.sort

createWriteFile :: MonadUnliftIO m => FilePath -> Text -> m ()
createWriteFile p t = do
  createDirectoryIfMissing True $ takeDirectory p
  writeFileUtf8 p t

displayText :: Text -> Utf8Builder
displayText = displayBytesUtf8 . encodeUtf8

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

setThreads :: MonadUnliftIO m => Int -> m ()
setThreads n
  | n > 0 = setNumCapabilities n
  | otherwise = setNumCapabilities =<< liftIO getNumProcessors
