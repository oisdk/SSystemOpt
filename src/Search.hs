{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Search
       ( search
       ) where

import           Control.Applicative (liftA2)
import           Control.Monad
import           Data.Foldable
import           Data.Function       (on)
import           Data.List           (sortOn)
import qualified Data.Map            as Map
import           Utils               hiding (zipWith)


-- eachNext :: Int -> a -> a -> [[a]]
-- eachNext m a b = f m where
--   f 0 = [[]]
--   f n = [ x:xs | x <- [a,b], xs <- f (n-1) ]

eachOf :: [[a]] -> [[a]]
eachOf = foldr (liftA2 (:)) [[]]

groupWithPos :: Ord a => [a] -> Map.Map a [Integer]
groupWithPos = foldr f Map.empty . zip [0..] where
  f (v,k) = Map.alter (Just . maybe [v] (v:)) k

searchGrouped :: Map.Map [a] [Integer] -> [[a]]
searchGrouped = map (map fst . sortOn snd . ((\(h,t) -> map ((,) h) t) =<<)) . eachOf . map (\(h,t) -> map (flip (,) t) h) . Map.assocs

lattice :: Ord a => [[a]] -> [[a]]
lattice = searchGrouped . groupWithPos

-- lattice' :: Ord a => [[a]] -> [[a]]
-- lattice' = map (map snd . sortOn fst) . Map.foldrWithKey f [[]] . groupWithPos where
--   f k v b =  [ zip v h ++ t | hs <- (zipWith ((eachNext.length) v) <*> tail) k, h <- hs, t <- b]


rmse :: [[Double]] -> [[Double]] -> Double
rmse a b = (sqrt . mean) (zipWith f a b) where
  f x y = mean $ zipWith (\n m -> sqr (n-m) ) x y

mean :: Foldable f => f Double -> Double
mean = uncurry (/) . foldl' (\(n,d) e -> (n + e, d + 1)) (0,0)

sqr :: Num a => a -> a
sqr x = x * x

search :: Monad m
       => ([Double] -> m (Maybe [[Double]])) -- ^ Simulation function
       -> [[Double]]                 -- ^ Observed data
       -> [[Double]]                 -- ^ Parameters
       -> m (Maybe [Double])
search sim obs = maybe (pure Nothing) close <=< (minOnA metFnc . lattice) where
  metFnc = fmap (MaxMaybe . fmap (rmse obs)) . sim
  close = go [] where
    go ys [] = pure $ Just ys
    go ys (x:xs) = maybe (pure Nothing) (`go` xs) =<< prepWith (around x) where
      around y = minOnA (metFnc . (\h -> ys ++ (h:xs))) [y-0.5, y, y+0.5]
      prepWith = (fmap.fmap) (\y -> ys ++ [y])

newtype MaxMaybe a = MaxMaybe { getMaxMaybe :: Maybe a
                              } deriving (Functor, Eq)

instance Ord a => Ord (MaxMaybe a) where
  compare = cmp `on` getMaxMaybe where
    cmp Nothing Nothing = EQ
    cmp (Just a) (Just b) = compare a b
    cmp (Just _) _ = LT
    cmp _ (Just _) = GT
