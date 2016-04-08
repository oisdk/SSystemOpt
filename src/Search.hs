module Search
       ( lattice
       , minByM
       , rmse
       ) where

import qualified Data.Map as Map
import Data.List (sortOn)
import Data.Foldable
import Utils


eachNext :: Int -> a -> a -> [[a]]
eachNext x a b = f x where
  f 0 = [[]]
  f n = map (a:) r ++ map (b:) r where r = f (n-1)

groupWithPos :: Ord a => [a] -> Map.Map a [Integer]
groupWithPos = foldr f Map.empty . zip [0..] where
  f (v,k) = Map.insertWith (++) k [v]

lattice :: Ord a => [[a]] -> [[a]]
lattice = map (map snd . sortOn fst) . Map.foldrWithKey f [[]] . groupWithPos where
  f k v b =  [ zip v h ++ t | hs <- (zipWith ((eachNext.length) v) <*> tail) k, h <- hs, t <- b]

minByM :: (Monad m, Foldable f) => (a -> a -> m Ordering) -> f a -> m (Maybe a)
minByM cmp = foldrM f Nothing where
  f e = fmap Just . maybe (pure e) (\a -> fmap (bool e a . (LT==)) (cmp e a))

rmse :: [[Double]] -> [[Double]] -> Double
rmse a b = (sqrt . mean) (zipWith f a b) where
  f x y = mean $ zipWith (\n m -> sqr (n-m) ) x y

mean :: Foldable f => f Double -> Double
mean = uncurry (/) . foldl' (\(n,d) e -> (n + e, d + 1)) (0,0)

sqr :: Num a => a -> a
sqr x = x * x
