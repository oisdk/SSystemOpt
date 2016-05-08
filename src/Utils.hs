{-# LANGUAGE DeriveFunctor #-}

module Utils where

import           Control.Applicative
import           Control.Arrow       (first)
import           Control.Monad       ((<=<))
import           Data.Foldable
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (pack)
import           Turtle              (Shell, die)

insertUnique :: Ord k => k -> a -> Map k a -> Either k (Map k a)
insertUnique key val m = case Map.insertLookupWithKey (\_ n _ -> n) key val m of
  (Just _,_) -> Left key
  (Nothing,n) -> Right n

sortUniques :: (Foldable f, Ord a) => f (a, b) -> Either a [(a,b)]
sortUniques = fmap Map.assocs . foldrM (uncurry insertUnique) Map.empty

mergeMatch :: Ord k
           => (a -> b -> c)
           -> Map k a
           -> Map k b
           -> Either [k] (Map.Map k c)
mergeMatch f x y = case Map.keys (symmetricDifference x y) of
  [] -> Right (Map.intersectionWith f x y)
  xs -> Left xs

bool :: a -> a -> Bool -> a
bool t _ True  = t
bool _ f False = f

uniqNames :: [String]
uniqNames = flip (:) <$> [] : uniqNames <*> ['a'..'z']

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = Left <$> x <|> Right <$> y

toDie :: Either String a -> Shell a
toDie = either (die . pack) pure

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA m x = go m where
  go 0 = pure []
  go n = (:) <$> x <*> go (n-1)

minOnA :: (Foldable t, Applicative f, Ord b) => (a -> f b) -> t a -> f (Maybe a)
minOnA cnv = (fmap.fmap) fst . foldr f (pure Nothing) where
  f e a = g e <$> cnv e <*> a
  g e x = Just . maybe (e,x) (uncurry (h e x))
  h e x a y = case compare x y of
    GT -> (a,y)
    _  -> (e,x)

minByM :: (Foldable f, Monad m) => (a -> a -> m Ordering) -> f a -> m (Maybe a)
minByM cmp = foldr f (pure Nothing) where
  f e a = fmap Just . maybe (pure e) (g e) =<< a
  g e a = c <$> cmp e a where
    c LT = e
    c EQ = e
    c GT = a

symmetricDifference :: Ord d
                    => Map d a
                    -> Map d b
                    -> Map d (Either a b)
symmetricDifference = Map.mergeWithKey (\_ _ _ -> Nothing) (Map.map Left) (Map.map Right)

newtype Source s a =
  Source { runSource :: [s] -> Maybe (a, [s])
         } deriving (Functor)

pop :: Source s s
pop = Source uncons where
  uncons [] = Nothing
  uncons (x:xs) = Just (x, xs)

instance Applicative (Source s) where
  pure x = Source (\s -> Just (x, s))
  Source f <*> Source x =
    Source ((\(g,s) -> first g <$> x s) <=< f)

evalSource :: Source s a -> [s] -> Maybe a
evalSource s = fmap fst . runSource s
