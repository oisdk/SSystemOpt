{-# LANGUAGE DeriveFunctor     #-}

module Zipper where

import Data.Foldable
import Control.Comonad
import Control.Applicative.Backwards

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

data Zipper a = Z [a] a [a] deriving Functor

instance Foldable Zipper where
  foldr f i (Z xs x ys) = foldl' (flip f) (f x (foldr f i ys)) xs
  
instance Traversable Zipper where
  traverse f (Z xs x ys) = Z <$> forwards (traverse (Backwards . f) xs) 
                               <*> f x 
                               <*> traverse f ys

instance Show a => Show (Zipper a) where
  showsPrec _ (Z xs x ys) = fnt . mid . bck where
    fnt = showChar '[' . (joinC . reverse) xs
    mid = showString " | " . shows x . showString " | " 
    bck = joinC ys . showChar ']'
    joinC [] = id
    joinC (x:xs) = shows x . foldr (\e a -> showString ", " . shows e . a) id xs

fromList :: [a] -> Zipper a
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
children z@(Z _ x _) = next x ?? next (succ x) where
  Z [] _ [] ?? x = x
  x          ?? _ = x
  next n = divg ((n>=) . extract) (centre succ <<= z)
  divg p (Z xs y ys) = Z (filter p xs) z (filter p (y:ys))

centre :: (a -> a) -> Zipper a -> Zipper a
centre f (Z xs x ys) = Z xs (f x) ys

iterEnd :: (a -> Maybe a) -> a -> [a]
iterEnd f = g where g x = x : maybe [] g (f x)
 
search :: (Enum a, Eq a, Ord a) => (Zipper a -> Zipper a -> Ordering) -> Zipper a -> Zipper a
search cmp = search' where
  search' = maxFind . children
  maxFind (Z xs x ys) = maybe x (dive x) (maxMaybe (maxMaybe Nothing ys) xs)
  dive x y = bool x (search' y) (LT /= cmp x y)
  maxMaybe = foldr (\e -> Just . maybe e (bool e <*> (GT==) . cmp e))
