{-# LANGUAGE TupleSections #-}

module Utils where

import           Control.Monad.State
import           Data.Bifunctor      (bimap)
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import           Data.Maybe

unzipWith :: (a -> (b,c)) -> [a] -> ([b],[c])
{-# INLINE unzipWith #-}
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
uniqName = State (swap . fromJust . List.uncons)

untilM :: Monad m => (a -> m (Maybe a)) -> a -> m a
untilM f = g where g x = f x >>= maybe (pure x) g
