{-# LANGUAGE DeriveAnyClass #-}

module Internal.Types.BiNonEmpty where

import Data.Semigroup (sconcat)
import RIO
import qualified RIO.NonEmpty as N

-- | A non-empty list with two types in it
data BiNonEmpty a b = BiNonEmpty
  { bneHead :: Either a b
  , bneLeft :: [a]
  , bneRight :: [b]
  }

instance Bifunctor BiNonEmpty where
  bimap f g (BiNonEmpty h as bs) =
    BiNonEmpty (bimap f g h) (fmap f as) (fmap g bs)

instance Bifoldable BiNonEmpty where
  bifoldMap f g (BiNonEmpty h as bs) = mconcat $ case h of
    Left h' -> f h' : as' ++ bs'
    Right h' -> as' ++ g h' : bs'
    where
      as' = f <$> as
      bs' = g <$> bs

deriving instance Bitraversable BiNonEmpty

groupWith :: Eq c => (a -> c) -> BiNonEmpty a b -> BiNonEmpty (NonEmpty a) b
groupWith f = withFirst (N.groupWith f) (N.groupWith1 f)

flatten :: BiNonEmpty (a, NonEmpty b) c -> BiNonEmpty (a, b) c
flatten = withFirst (concatMap (go . second N.toList)) (sconcat . fmap go)
  where
    go (x, ys) = (x,) <$> ys

concatMapBNE :: (a -> NonEmpty b) -> BiNonEmpty a c -> BiNonEmpty b c
concatMapBNE f = concatBNE . first f

concatBNE :: BiNonEmpty (NonEmpty a) b -> BiNonEmpty a b
concatBNE = withFirst (concatMap N.toList) sconcat

swap :: BiNonEmpty a b -> BiNonEmpty b a
swap (BiNonEmpty h as bs) = BiNonEmpty (either Right Left h) bs as

fromNonEmpty :: (a -> Either b c) -> NonEmpty a -> BiNonEmpty b c
fromNonEmpty f (x :| xs) = BiNonEmpty (f x) as bs
  where
    (as, bs) = partitionEithers (f <$> xs)

fromList :: [a] -> [b] -> Maybe (BiNonEmpty a b)
fromList [] [] = Nothing
fromList (a : as) bs = Just $ BiNonEmpty (Left a) as bs
fromList as (b : bs) = Just $ BiNonEmpty (Right b) as bs

foldToNonEmpty :: (a -> c) -> (b -> c) -> BiNonEmpty a b -> NonEmpty c
foldToNonEmpty f g = toNonEmpty . bimap f g

toNonEmpty :: BiNonEmpty a a -> NonEmpty a
toNonEmpty (BiNonEmpty h as bs) = case h of
  Left h' -> let d = (h' :| as) in maybe d (d <>) $ N.nonEmpty bs
  Right h' -> let d = (h' :| bs) in maybe d (<> d) $ N.nonEmpty as

withFirst :: ([a] -> [c]) -> (NonEmpty a -> NonEmpty c) -> BiNonEmpty a b -> BiNonEmpty c b
withFirst f g (BiNonEmpty h as bs) = case h of
  Right h' -> BiNonEmpty (Right h') (f as) bs
  Left h' ->
    let (h'' :| as') = g (h' :| as)
     in BiNonEmpty (Left h'') as' bs
