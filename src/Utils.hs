{-# LANGUAGE BangPatterns #-}

module Utils where

import           Control.Applicative
import           Control.Monad.State
import           Data.Bifunctor      (bimap)
import           Data.List           (sortOn)
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import Data.Foldable
import Data.Traversable

unzipWith :: (a -> (b,c)) -> [a] -> ([b],[c])
unzipWith f = foldr (uncurry bimap . bimap (:) (:) . f) ([],[])

clines :: [String] -> String
clines xs = foldr f id xs "" where
  f e a = showString e . showString ";\n" . a

insertUnique :: Ord k => k -> a -> Map.Map k a -> Maybe (Map.Map k a)
insertUnique key val m = case Map.insertLookupWithKey (\_ n _ -> n) key val m of
  (Just _,_) -> Nothing
  (Nothing,n) -> Just n

symmetricDifferenceWith :: Ord k
                        => (a -> c)
                        -> (b -> c)
                        -> Map.Map k a
                        -> Map.Map k b
                        -> Map.Map k c
symmetricDifferenceWith f g = Map.mergeWithKey (\_ _ _ -> Nothing) (Map.map f) (Map.map g)

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

iterEnd :: (a -> Maybe a) -> a -> [a]
iterEnd f = g where g x = x : maybe [] g (f x)

ensure :: (a -> Bool) -> a -> Maybe a
ensure p x = bool (Just x) Nothing (p x)

infixr 9 .:
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) g f x y = g (f x y)

uniqNames :: [String]
uniqNames = flip (:) <$> [] : uniqNames <*> ['a'..'z']

uniqName :: State [String] String
uniqName = state (fromJust . List.uncons)

untilM :: Monad m => (a -> m (Maybe a)) -> a -> m a
untilM f = g where g x = f x >>= maybe (pure x) g

newtype RecFR a ans = RecFR { unRecFR :: a -> (RecFR a ans -> ans) -> ans }

zipWithF :: (Foldable f, Foldable g) => (a -> b -> c) -> f a -> g b -> [c]
zipWithF f = foldr2 (\a b c -> f a b : c) []

foldr2 :: (Foldable f, Foldable g) => (a -> b -> c -> c) -> c -> f a -> g b -> c
foldr2 f i xs = foldr g (const i) xs . RecFR . foldr h ((const.const) i) where
  g e r x = unRecFR x e r
  h e2 r2 e1 r1 = f e1 e2 (r1 (RecFR r2))

zipF :: (Foldable f, Foldable g) => f a -> g b -> [(a, b)]
zipF = zipWithF (,)

infixl 2 ??
(??) :: Alternative f => f a -> a -> f a
(??) a b = a <|> pure b

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = Left <$> x <|> Right <$> y

newtype RecAccu a b = RecAccu { unRecAccu :: a -> (RecAccu a b, b) }

zipInto :: (Traversable t, Foldable f) => (a -> Maybe b -> c) -> t a -> f b -> t c
zipInto f xs = snd . flip (mapAccumL unRecAccu) xs . RecAccu . foldr h i where
  i e = (RecAccu i, f e Nothing)
  h e2 a e1 = (RecAccu a, f e1 (Just e2))

minByM :: (Monad m, Foldable f) => (a -> a -> m Ordering) -> f a -> m (Maybe a)
minByM cmp = foldrM f Nothing where
  f e = fmap Just . maybe (pure e) (\a -> fmap (bool e a . (LT==)) (cmp e a))

mse :: [Double] -> [Double] -> Double
mse = sqrt .: uncurry (/) .: foldl' sumInc (0,0) .: zipWith sqDiff where
  sqDiff !a !b = let d = a - b in d * d
  sumInc (!s,!n) !e = (s+e, n+1)

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

right :: (a -> b) -> Either a b -> b
right f (Left x) = f x
right _ (Right x) = x

left :: (a -> b) -> Either b a -> b
left _ (Left x) = x
left f (Right x) = f x

replace :: (Traversable t, Foldable f) => (a -> Maybe b) -> (a -> b) -> t a -> f b -> t b
replace f g xs = snd . flip (mapAccumL unRecAccu) xs . RecAccu . foldr h i where
  i e = (RecAccu i, g e)
  h e2 a = maybe (RecAccu a, e2) (RecAccu (h e2 a),) . f
