module Internal.Utils where

import Control.Monad.Error.Class
import Data.Scientific
import Internal.Types.Main
import RIO
import qualified RIO.List as L
import RIO.State
import qualified RIO.Text as T

throwAppError :: MonadAppError m => AppError -> m a
throwAppError e = throwError $ AppException [e]

throwAppErrorIO :: MonadUnliftIO m => AppError -> m a
throwAppErrorIO = fromEither . throwAppError

throwAppWarning :: NutrientState m => AppWarning -> m ()
throwAppWarning w = modify $ \s -> s {fsWarnings = w : fsWarnings s}

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

showError :: AppError -> [T.Text]
showError other = case other of
  (NutrientError) -> undefined -- [T.unwords ["could not parse valud for nutrient id:", tshow i]]
  (DaySpanError d) -> [T.unwords ["time interval must be positive, got", tshow d, "days"]]
  (UnitParseError u) -> [T.append "could not parse unit: " u]
  (UnitMatchError x y) ->
    [ T.append
        (T.unwords ["could not add", tshow x, "and", tshow y])
        ": units don't match"
    ]
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
      "J" -> return $ Unit p Joule
      "IU" -> return $ Unit p IU
      _ -> Nothing

-- parseUnit :: MonadAppError m => T.Text -> m Unit
-- parseUnit s = catchError nonUnity (const def)
--   where
--     def = parseName Unity s
--     nonUnity = case T.splitAt 1 s of
--       ("G", rest) -> parseName Giga rest
--       ("M", rest) -> parseName Mega rest
--       ("k", rest) -> parseName Kilo rest
--       ("h", rest) -> parseName Hecto rest
--       ("d", rest) -> parseName Deci rest
--       ("c", rest) -> parseName Centi rest
--       ("m", rest) -> parseName Milli rest
--       ("µ", rest) -> parseName Micro rest
--       ("n", rest) -> parseName Nano rest
--       _ -> case T.splitAt 2 s of
--         ("da", rest) -> parseName Deca rest
--         _ -> def
--     parseName p r = case r of
--       "cal" -> return $ Unit p Calorie
--       "g" -> return $ Unit p Gram
--       "J" -> return $ Unit p Joule
--       "IU" -> return $ Unit p IU
--       _ -> throwAppError $ UnitParseError s

raisePower :: Int -> Scientific -> Scientific
raisePower x s = scientific (coefficient s) (base10Exponent s + x)
