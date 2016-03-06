{-# LANGUAGE TupleSections #-}

module Utils
  ( unzipWith
  , clines
  , State(..)
  , get
  , put
  , evalState
  , insertUnique
  , symmetricDifferenceWith
  , bool
  , iterEnd
  , ensure
  ) where

import           Data.Bifunctor (bimap)
import qualified Data.Map.Strict as Map

unzipWith :: (a -> (b,c)) -> [a] -> ([b],[c])
{-# INLINE unzipWith #-}
unzipWith f = foldr (uncurry bimap . bimap (:) (:) . f) ([],[])

clines :: [String] -> String
clines xs = foldr f id xs "" where
  f e a = showString e . showString ";\n" . a

newtype State s a = State { runState :: s -> (s, a) }

instance Functor (State s) where
  fmap f (State s) = State (fmap f . s)

instance Applicative (State s) where
  pure x = State (,x)
  State f <*> State x = State (uncurry (flip fmap . x) . f)

instance Monad (State s)  where
  State x >>= f = State ((\(s,y) -> runState (f y) s) . x)

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (const (s, ()))

evalState :: s -> State s a -> a
evalState s = snd . flip runState s

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
