{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Utils where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.Foldable
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Text           (pack)
import           Turtle              (Shell, die)

-- | Inserts a value into a map, only if it's not already present
-- >>> insertUnique "a" "b" (Map.fromList [])
-- Right (fromList [("a","b")])
-- >>> insertUnique "a" "b" (Map.fromList [("a", "b")])
-- Left "a"
insertUnique :: Ord k => k -> a -> Map k a -> Either k (Map k a)
insertUnique key val m = case Map.insertLookupWithKey (\_ n _ -> n) key val m of
  (Just _,_) -> Left key
  (Nothing,n) -> Right n

-- | Sorts a list of tuples, returning left if any keys
-- are repeated
-- >>> sortUniques [(3,2),(1,4)]
-- Right [(1,4),(3,2)]
-- >>> sortUniques [(3,2),(3,5)]
-- Left 3
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

uniqNames :: Stream String
uniqNames = foldr Stream undefined un' where
  un' = flip (:) <$> [] : un' <*> ['a'..'z']

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = Left <$> x <|> Right <$> y

toDie :: Either String a -> Shell a
toDie = either (die . pack) pure

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

data Stream a = Stream
  { _streamHead :: a
  , _streamTail :: Stream a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Stream

instance Cons (Stream a) (Stream b) a b where
  _Cons = iso (\(Stream x xs) -> (x,xs)) (uncurry Stream)

data SourceState s = SourceState
  { _given  :: [s]
  , _source :: Stream s
  } deriving (Functor, Foldable, Traversable)

makeLenses ''SourceState

pop :: MonadState (SourceState s) m => m s
pop = do
  x <- use (source.streamHead)
  source %= view streamTail
  given %= (|> x)
  pure x

type Source s = State (SourceState s)

evalUniques :: Source String a -> a
evalUniques = flip evalState (SourceState [] uniqNames)
