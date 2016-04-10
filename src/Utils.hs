module Utils
       ( sortUniques
       , bool
       , uniqNames
       , eitherA
       , toDie
       , eitherToMaybe
       ) where

import           Control.Applicative
import qualified Data.Map.Strict     as Map
import           Data.Text           (pack)
import           Turtle              (Shell, die)

insertUnique :: Ord k => k -> a -> Map.Map k a -> Either k (Map.Map k a)
insertUnique key val m = case Map.insertLookupWithKey (\_ n _ -> n) key val m of
  (Just _,_) -> Left key
  (Nothing,n) -> Right n

sortUniques :: (Foldable f, Ord a) => f (a, b) -> Either a [(a,b)]
sortUniques = fmap Map.assocs . foldr ((=<<) . uncurry insertUnique) (Right Map.empty)

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
