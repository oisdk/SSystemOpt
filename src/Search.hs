{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Search where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Experiments
import Data.List
import Statistics.Matrix (Matrix, fromRowLists)
import Numeric.Expr
import SSystem

almostEqual :: Double -> Double -> Bool
almostEqual x y = abs (x-y) < 0.00001

variableDataStepSize :: VariableData -> Maybe Double
variableDataStepSize vd = vd ^.. (vals.each.time) & equalDiffs almostEqual

networkStepSize :: Network -> Maybe Double
networkStepSize = views variables (concatIfEqual almostEqual <=< traverse variableDataStepSize)

experimentStepSize :: Experiment -> Maybe Double
experimentStepSize = views networks (concatIfEqual almostEqual <=< traverse networkStepSize)

stopTime :: Experiment -> Double
stopTime expr = last (head (head (expr^.networks) ^. variables) ^. vals) ^. time

nsteps :: Experiment -> Double
nsteps expr = let Just ss = experimentStepSize expr in stopTime expr / ss

data EqualAccum a
  = Started
  | NotEqual
  | Equal a

-- |
-- >>> concatIfEqual (==) []
-- Nothing
-- >>> concatIfEqual (==) [1]
-- Just 1
-- >>> concatIfEqual (==) [1,1]
-- Just 1
-- >>> concatIfEqual (==) [1,2]
-- Nothing

concatIfEqual :: (Foldable f, Eq a, Show a) => (a -> a -> Bool) -> f a -> Maybe a
concatIfEqual eq = ext . foldl' f Started where
  f Started e   = Equal e
  f NotEqual _  = NotEqual
  f (Equal a) e
    | eq e a    = Equal a
    | otherwise = NotEqual
  ext (Equal a) = pure a
  ext Started   = Nothing
  ext NotEqual  = Nothing

-- |
-- >>> equalDiffs (==) [1..10]
-- Just 1
-- >>> equalDiffs (==) [1, 3..11]
-- Just 2
-- >>> equalDiffs (==) [1, 3, 4]
-- Nothing
-- >>> equalDiffs (==) []
-- Nothing
equalDiffs :: (Eq a, Num a, Show a) => (a -> a -> Bool) -> [a] -> Maybe a
equalDiffs _ [] = Nothing
equalDiffs eq xs = concatIfEqual eq (zipWith (-) (tail xs) xs)

toFlatList :: Network -> [Double]
toFlatList net = join $ transpose $ map (map (view value) . view vals) $ net ^. variables

toPredictors :: Experiment -> Matrix
toPredictors = fromRowLists . views networks (map toFlatList)

versions :: SSystem (Either (VarExpr Double) (Double,Double)) -> [SSystem (VarExpr Double)]
versions = traverse (either f g) where
  f = pure
  g (l,t) = map Lit [l, l + 0.5 .. t]
