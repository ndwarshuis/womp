module Internal.Utils where

import Control.Monad.Error.Class
import Data.Scientific
import Internal.Types.Dhall
import Internal.Types.Main
import RIO
import RIO.FilePath
import qualified RIO.List as L
import qualified RIO.NonEmpty as N
import qualified RIO.Text as T
import RIO.Time
import UnliftIO.Directory

throwAppError :: MonadAppError m => AppError -> m a
throwAppError e = throwError $ AppException [e]

throwAppErrorIO :: MonadUnliftIO m => AppError -> m a
throwAppErrorIO = fromEither . throwAppError

-- throwAppWarning :: NutrientState m => NutrientWarning -> m ()
-- throwAppWarning w =
--   modify $ \s -> s {fsWarnings = w : fsWarnings s}

combineError3
  :: (Semigroup e, MonadError e m)
  => m a
  -> m b
  -> m c
  -> (a -> b -> c -> d)
  -> m d
combineError3 a b c f =
  combineError (combineError a b (,)) c $ \(x, y) z -> f x y z

combineError
  :: (Semigroup e, MonadError e m)
  => m a
  -> m b
  -> (a -> b -> c)
  -> m c
combineError a b f = combineErrorM a b (\x y -> pure $ f x y)

combineErrorM
  :: (Semigroup e, MonadError e m)
  => m a
  -> m b
  -> (a -> b -> m c)
  -> m c
combineErrorM a b f = do
  a' <- catchError a $ \e ->
    throwError =<< catchError (e <$ b) (return . (e <>))
  f a' =<< b

-- liftExcept :: MonadError e m => Except e a -> m a
-- liftExcept = either throwM return . runExcept

mapErrors :: (Traversable t, MonadAppError m) => (a -> m b) -> t a -> m (t b)
-- First, record number of each action. Then try each action. On first failure,
-- note it's position in the sequence, skip ahead to the untried actions,
-- collect failures and add to the first failure.
mapErrors f xs = mapM go $ enumTraversable xs
  where
    go (n, x) = catchError (f x) $ \e -> do
      es <- fmap catMaybes $ mapM (err . f) $ drop (n + 1) $ toList xs
      throwError $ foldr (<>) e es
    err x = catchError (Nothing <$ x) (pure . Just)

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

showError :: AppError -> [T.Text]
showError other = case other of
  (MissingCustom c) -> [T.append "Custom ingredient not found: " c]
  (CustomIngError c) -> [fmtCustomError c]
  (FileTypeError f) -> [T.append "File must be .yml/yaml or .dhall: " $ T.pack f]
  (JSONError e) -> [T.append "JSON parse error: " $ decodeUtf8Lenient e]
  (EmptyMeal n) -> [T.append "Meal has no ingredients: " n]
  (MissingAPIKey p) -> [T.append "Could not read API key from path: " $ T.pack p]
  (DaySpanError d) -> [T.unwords ["day interval must be positive, got", tshow d, "days"]]
  (IntervalError d) -> [T.unwords ["aggregation interval must be positive, got", tshow d, "days"]]
  (DatePatternError s b r p) -> [T.unwords [msg, "in pattern: ", pat]]
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

keyVals :: [(T.Text, T.Text)] -> T.Text
keyVals = T.intercalate "; " . fmap (uncurry keyVal)

keyVal :: T.Text -> T.Text -> T.Text
keyVal a b = T.concat [a, "=", b]

parseUnit :: T.Text -> Maybe Unit
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

raisePower :: Int -> Scientific -> Scientific
raisePower x s = scientific (coefficient s) (base10Exponent s + x)

roundDigits :: forall a. RealFrac a => Int -> a -> a
roundDigits p = (/ d) . go . round . (* d)
  where
    d = 10 ^ p
    go :: Int -> a
    go = fromIntegral

autoPrefix :: Scientific -> Prefix
autoPrefix s =
  maybe maxBound fst $
    L.find ((abs s <) . snd) $
      fmap
        (\p -> (p,) $ (10 ^^) $ (+ 3) $ prefixValue p)
        -- NOTE only use multiples of three for display
        [Nano, Micro, Milli, Unity, Kilo, Mega, Giga]

withNonEmpty :: (Eq a, Monoid a) => (a -> b) -> b -> a -> b
withNonEmpty f d x
  | mempty == x = d
  | otherwise = f x

-- NOTE this won't work for negative denominators
divSci :: Integral n => Scientific -> n -> Scientific
divSci n d
  | n == 0 = 0
  | otherwise = scientific c1 (base10Exponent n - pd)
  where
    c0 = coefficient n
    c1 = div (c0 * 10 ^ pd) $ fromIntegral d
    pd = ceiling $ logBase 10 (fromIntegral d :: Double)

-- TODO could make this more general...if I feel like it
findDups :: Ord a => [a] -> [a]
findDups = fmap N.head . filter ((> 1) . length) . N.group . L.sort

cronPatLength :: Num a => DaySpan -> Cron -> a
cronPatLength ds = fromIntegral . length . expandCronPat ds

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

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

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

dayToWeekday :: Day -> Int
dayToWeekday (ModifiedJulianDay d) = mod (fromIntegral d + 2) 7

maybeExit
  :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m)
  => Text
  -> Maybe a
  -> m a
maybeExit msg = maybe (exitError msg) return

createWriteFile :: MonadUnliftIO m => FilePath -> Text -> m ()
createWriteFile p t = do
  createDirectoryIfMissing True $ takeDirectory p
  writeFileUtf8 p t

exitError :: (MonadReader env m, HasLogFunc env, MonadUnliftIO m) => Text -> m a
exitError msg = logError (displayText msg) >> exitFailure

displayText :: Text -> Utf8Builder
displayText = displayBytesUtf8 . encodeUtf8
