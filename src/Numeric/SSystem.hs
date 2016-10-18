{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Numeric.SSystem where

import           Control.Lens
import           Data.Foldable
import           Data.List           (zipWith4)
import           Data.Sequence       (Seq)
import qualified Data.Sequence       as Seq
import           Data.Utils
import           Text.Parse.ODEBench

data SODE a = SODE
  { _alpha  :: a
  , _posExp :: Seq a
  , _beta   :: a
  , _negExp :: Seq a
  } deriving (Functor, Foldable, Traversable, Eq, Ord)

data SSystem a = SSystem
  { _odes :: Seq (SODE a)
  , _size :: Int
  } deriving (Functor, Foldable, Traversable, Eq, Ord)

instance FoldableWithIndex Int SSystem
instance TraversableWithIndex Int SSystem
instance FunctorWithIndex Int SSystem
type instance Index (SSystem a) = Int
type instance IxValue (SSystem a) = a


instance Show a => Show (SSystem a) where
  showsPrec _ SSystem {..} = showTable 4 2
    (2 + _size * 2)
    (["a"] ++ [ 'g' : show i | i <- [1.._size]] ++ ["b"] ++ [ 'h' : show i | i <- [1.._size]])
    (flip imap _odes $ \i SODE {..} -> (show (succ i), _alpha : foldr (:) (_beta : foldr (:) [] _negExp) _posExp))

toEqn' :: SODE Double -> [Double] -> Double
toEqn' SODE {..} v =
  foldl' (*) _alpha (zipWith (**) v (foldr (:) [] _posExp)) -
  foldl' (*) _beta  (zipWith (**) v (foldr (:) [] _negExp))

toEqns' :: SSystem Double -> [Double] -> [Double]
toEqns' SSystem {..} = traverse toEqn' (foldr (:) [] _odes)

fromParams :: ParamInfo a -> SSystem a
fromParams space = SSystem (toOde space) len where
  len = length (alphas space)
  toOde ParamInfo {..} = Seq.fromList $
    zipWith4 SODE
      alphas
      (map Seq.fromList posExps)
      betas
      (map Seq.fromList negExps)

makeLenses ''SODE
makeLenses ''SSystem

instance Ixed (SSystem a) where
  ix i f s
    | x == 0 = (odes. ix y . alpha) f s
    | x <= _size s = (odes . ix y . posExp . ix (x-1)) f s
    | x == _size s + 1 = (odes . ix y . beta) f s
    | otherwise = (odes . ix y . negExp . ix (x - _size s - 2)) f s
    where (y,x) = i `quotRem` _size s
-- | >>> exampleSystem
-- ┌──┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┬────┐
-- │  │ a  │ g1 │ g2 │ g3 │ g4 │ g5 │ b  │ h1 │ h2 │ h3 │ h4 │ h5 │
-- ├──┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┼────┤
-- │ 1│ 5.0│ 0.0│ 0.0│ 1.0│ 0.0│-1.0│10.0│ 2.0│ 0.0│ 0.0│ 0.0│ 0.0│
-- │ 2│10.0│ 2.0│ 0.0│ 0.0│ 0.0│ 0.0│10.0│ 0.0│ 2.0│ 0.0│ 0.0│ 0.0│
-- │ 3│10.0│ 0.0│-1.0│ 0.0│ 0.0│ 0.0│10.0│ 0.0│-1.0│ 2.0│ 0.0│ 0.0│
-- │ 4│ 8.0│ 0.0│ 0.0│ 2.0│ 0.0│-1.0│10.0│ 0.0│ 0.0│ 0.0│ 2.0│ 0.0│
-- │ 5│10.0│ 0.0│ 0.0│ 0.0│ 2.0│ 0.0│10.0│ 0.0│ 0.0│ 0.0│ 0.0│ 2.0│
-- └──┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┴────┘
exampleSystem :: SSystem Double
exampleSystem = SSystem
  [ SODE 5  [ 0, 0, 1, 0,-1] 10 [ 2, 0, 0, 0, 0]
  , SODE 10 [ 2, 0, 0, 0, 0] 10 [ 0, 2, 0, 0, 0]
  , SODE 10 [ 0,-1, 0, 0, 0] 10 [ 0,-1, 2, 0, 0]
  , SODE 8  [ 0, 0, 2, 0,-1] 10 [ 0, 0, 0, 2, 0]
  , SODE 10 [ 0, 0, 0, 2, 0] 10 [ 0, 0, 0, 0, 2]
  ] 5

alphal, betal :: Int -> (forall a. Traversal' (SSystem a) a)
alphal i = odes . ix i . alpha
betal  i = odes . ix i . beta

posExpl, negExpl :: Int -> Int -> (forall a. Traversal' (SSystem a) a)
posExpl i j = odes . ix i . posExp . ix j
negExpl i j = odes . ix i . negExp . ix j

