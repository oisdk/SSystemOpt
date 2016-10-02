{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Search where

import           Control.Lens
import           Control.Monad
import           Data.Foldable
import           Experiments
import Data.List
import Statistics.Matrix (Matrix, Vector)
import Statistics.Matrix (Matrix, Vector)
import Numeric.Expr
import SSystem
import Data.Vector.Unboxed (fromList)
import qualified Data.Set as Set
import GHC.Exts (fromString)
import qualified Data.Sequence as Seq
import Utils hiding (zipWith, ordNub)

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
nsteps expr = let Just ss = experimentStepSize expr in 1 + stopTime expr / ss

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

toPredictors :: Experiment -> [Vector]
toPredictors = views networks (map (fromList . toFlatList))

versions :: SSystem (Either (VarExpr Double) (Double,Double)) -> [SSystem (VarExpr Double)]
versions = ordNub derivs . traverse (either f g) where
  f = pure
  g (l,t) = map Lit [l, l + 0.5 .. t]

ordNub :: Ord a => (b -> a) -> [b] -> [b]
ordNub o = f Set.empty where
  f _ [] = []
  f s (x:xs) | Set.member k s = f s xs
             | otherwise = x : f (Set.insert k s) xs
             where k = o x

derivs :: (Floating a, Eq a) => SSystem (VarExpr a) ->  [VarExpr a]
derivs s = foldr (:) [] $ toEqns (Var . fromString . Seq.index varNames) s where
  varNames = Seq.fromList (takeStream (size s) uniqNames)
