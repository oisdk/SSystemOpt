{-# LANGUAGE LambdaCase #-}

module Data.Utils where

import           Data.Foldable hiding (foldl1, foldr1)
import           Prelude       hiding (foldl1, foldr1)


foldr1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldr1 f = foldr g Nothing where g e = Just . maybe e (f e)

foldl1 :: Foldable f => (a -> a -> a) -> f a -> Maybe a
foldl1 f = foldl' (\a e -> Just . maybe e (`f` e) $ a) Nothing

maximumOn :: (Foldable f, Ord b) => (a -> b) -> f a -> Maybe a
maximumOn ord = fmap snd . foldl' f Nothing where
  f Nothing e = Just (ord e, e)
  f (Just (k,a)) e = Just $ case compare k n of
    LT -> (n,e)
    EQ -> (k,a)
    GT -> (k,a)
    where n = ord e

minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy cmp = foldl1 $ \a e -> case cmp a e of
  LT -> a
  EQ -> a
  GT -> e

maximumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy cmp = foldl1 $ \a e -> case cmp a e of
  LT -> e
  EQ -> a
  GT -> a

repC :: a -> Int -> [a] -> [a]
repC c m r = go m where
  go 0 = r
  go n = c : go (n-1)

repS :: [a] -> Int -> [a] -> [a]
repS s m r = go m where
  go 0 = r
  go n = s ++ go (n-1)

leftAlign :: Char -> Int -> String -> ShowS
leftAlign c m x xs =
  foldr f (flip (repC c) xs) x (max 0 m) where
    f _ _ 0 = xs
    f e a n = e : a (n-1)

rightAlign :: Char -> Int -> String -> ShowS
rightAlign c m x xs =
  uncurry ($) $ foldr f base x (max 0 m) where
    f _ _ 0 = (id,xs)
    f e a n = (e:) <$> a (n-1)
    base n = (repC c n,xs)

centreAlign :: Char -> Int -> String -> ShowS
centreAlign c m x xs =
  uncurry ($) $ foldr f base x (max 0 m) where
    f _ _ 0 = (id,xs)
    f e a n = (e:) <$> a (n-1)
    base n = (repC c l, repC c (n-l) xs) where
      l = n `div` 2


showTable :: (Show cellContents, Foldable f, Foldable g, Foldable h)
          => Int -- ^ Column width
          -> Int -- ^ Row header width
          -> Int -- ^ Number of columns
          -> f String -- ^ Column headers
          -> g (String, h cellContents)
          -> ShowS
showTable cw rw nc ch cn
  = showChar '┌'
  . repC '─' rw
  . repS ('┬' : replicate cw '─') nc
  . showString "┐\n│"
  . repC ' ' rw
  . flip (foldr $ \e -> ('│':) . centreAlign ' ' cw e) ch
  . showString "│\n├"
  . repC '─' rw
  . repS ('┼' : replicate cw '─') nc
  . showChar '┤'
  . flip (foldr $ \(t,c) ->
         showString "\n│" . rightAlign ' ' rw t
         . flip (foldr $ \e -> ('│':) . rightAlign ' ' cw (show e)) c
         . showChar '│') cn
  . showString "\n└"
  . repC '─' rw
  . repS ('┴' : replicate cw '─') nc
  . showChar '┘'
