{-# LANGUAGE DeriveFunctor #-}

module Zipper where

import           Control.Applicative.Backwards
import           Control.Comonad
import           Data.Foldable
import           Utils
import Data.Monoid

data Zipper a = Z [a] a [a] deriving (Functor, Eq)

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

fromList :: [a] -> Zipper a
fromList [] = error "Cannot make a zipper from an empty list"
fromList (x:xs) = Z [] x xs

left :: Zipper a -> Maybe (Zipper a)
left (Z [] _ _) = Nothing
left (Z (x:xs) y ys) = Just (Z xs x (y:ys))

right :: Zipper a -> Maybe (Zipper a)
right (Z _ _ []) = Nothing
right (Z xs x (y:ys)) = Just (Z (x:xs) y ys)

instance Comonad Zipper where
  extract (Z _ x _) = x
  duplicate z = Z (tail $ iterEnd left z) z (tail $ iterEnd right z)

children :: (Eq a, Enum a, Ord a) => Zipper a -> Zipper (Zipper a)
children z@(Z _ c _) = next c ?? next (succ c) where
  Z [] _ [] ?? x = x
  x         ?? _ = x
  next n = divg ((n>=) . extract) (centre succ <<= z)
  divg p (Z xs y ys) = Z (filter p xs) z (filter p (y:ys))

centre :: (a -> a) -> Zipper a -> Zipper a
centre f (Z xs x ys) = Z xs (f x) ys

search :: (Enum a, Eq a, Ord a) => (Zipper a -> Zipper a -> Ordering) -> Zipper a -> Zipper a
search cmp = search' where
  search' = maxFind . children
  maxFind (Z xs x ys) = maybe x search' (ensure ((LT==) . cmp x) =<< maxMaybe (maxMaybe Nothing ys) xs)
  maxMaybe = foldr (\e -> Just . maybe e (bool e <*> (GT==) . cmp e))
