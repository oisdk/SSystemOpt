{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE DeriveFunctor #-}
module Zipper where

import           Control.Applicative           (liftA2)
import           Control.Applicative.Backwards
import           Control.Comonad
import           Data.Foldable
import           Data.Ord                      (comparing)
import           Utils
data Zipper a = Z [a] a [a] deriving (Functor, Eq)

data Bounds a = Bounds { minB :: a
                       , maxB :: a
                       , sucB :: a -> a
                       }

instance Foldable Zipper where
  foldr f i (Z xs x ys) = foldl' (flip f) (f x (foldr f i ys)) xs

instance Traversable Zipper where
  traverse f (Z xs x ys) = Z <$> forwards (traverse (Backwards . f) xs)
                             <*> f x
                             <*> traverse f ys

instance Show a => Show (Zipper a) where
  showsPrec _ = show' . fmap shows where
    show' (Z ls c rs) = fnt . mid . bck where
      fnt = showChar '[' . (joinC . reverse) ls
      mid = showString " | " . c . showString " | "
      bck = joinC rs . showChar ']'
      joinC [] = id
      joinC (x:xs) = x . foldr (\e a -> showString ", " . e . a) id xs

instance Ord a => Ord (Zipper a) where
  compare = comparing toList

fromList :: [a] -> Zipper a
fromList [] = error "Cannot make a zipper from an empty list"
fromList (x:xs) = Z [] x xs

left :: Zipper a -> Maybe (Zipper a)
left (Z [] _ _) = Nothing
left (Z (x:xs) y ys) = Just (Z xs x (y:ys))

right :: Zipper a -> Maybe (Zipper a)
right (Z _ _ []) = Nothing
right (Z xs x (y:ys)) = Just (Z (x:xs) y ys)

insert :: a -> Zipper a -> Zipper a
insert x (Z xs y ys) = Z xs x (y:ys)

instance Comonad Zipper where
  extract (Z _ x _) = x
  duplicate z = Z (tail $ iterEnd left z) z (tail $ iterEnd right z)

children :: Eq a => Bounds a -> Zipper a -> Zipper (Zipper a)
children (Bounds _ m s) = liftA2 f (insert <*> extend (centre s)) extract where
  f z c = g (filterZ ((c==) . extract) z) where
    g (Z [] _ []) | c /= m = z
    g r = r

diverge :: (a -> a) -> Zipper a -> Zipper (Zipper a)
diverge f = extend (centre f)

centre :: (a -> a) -> Zipper a -> Zipper a
centre f (Z xs x ys) = Z xs (f x) ys

edges :: (a -> a) -> Zipper a -> Zipper a
edges f (Z xs x ys) = Z (map f xs) x (map f ys)

filterZ :: (a -> Bool) -> Zipper a -> Zipper a
filterZ p (Z xs x ys) = Z (filter p xs) x (filter p ys)

searchM :: (Enum a, Ord a, Monad m)
        => (Zipper a -> Zipper a -> m Ordering)
        -> Bounds a
        -> Zipper a
        -> m (Zipper a)
searchM cmp b = search where
  search = maxFind . children b
  maxM = foldrM f where
    f e a = do
      c <- cmp e a
      case c of
        GT -> search e
        _  -> pure a
  maxFind (Z xs x ys) =  maxM x ys >>= flip maxM xs

mse :: [Double] -> [Double] -> Double
mse = sqrt .: uncurry (/) .: foldl' sumInc (0,0) .: zipWith sqDiff where
  sqDiff !a !b = let d = a - b in d * d
  sumInc (!s,!n) !e = (s+e, n+1)

