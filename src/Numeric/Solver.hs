{-# LANGUAGE RecordWildCards #-}

module Numeric.Solver where

import           Control.Exception
import           Data.Function
import           Data.List
import qualified Data.Vector.Storable       as Storable
import           Numeric.GSL.ODE
import           Numeric.LinearAlgebra.Data
import           Numeric.SSystem
import           Text.Parse.ODEBench

type Objective = SSystem Double -> Double

logLikelihood :: Matrix Double -- ^ Observed values
              -> Matrix Double -- ^ Standard deviations
              -> Matrix Double -- ^ Simulated values
              -> Double
logLikelihood o d s =
  (-0.5) * Storable.sum
  (Storable.zipWith3 ll
   (flatten o)
   (flatten d)
   (flatten s))
  where ll obs stddev sim = ((sim - obs) / stddev) ** 2

data Experiment = Experiment
  { times           :: Storable.Vector Double
  , initalVals      :: [Double]
  , observedVals    :: Matrix Double
  , observedStddevs :: Matrix Double }

experiments :: Problem -> [Experiment]
experiments = map toExpr . groupBy ((==) `on` experimentNum) . samples where
  toExpr = Experiment <$> Storable.fromList . map time
                      <*> Storable.toList . values . head
                      <*> fromRows . map values
                      <*> fromRows . map stdDevs

solve :: SSystem Double -> [Double] -> Storable.Vector Double -> Matrix Double
solve = odeSolve . const . toEqns'

likelihood :: [Experiment] -> Objective
likelihood exprs syst = foldl' (+) 0 (map f exprs) where
  f Experiment {..} =
    logLikelihood
      observedVals
      observedStddevs
      (solve syst initalVals times)

type SafeObjective = SSystem Double -> IO (Either SomeException Double)

safeLikelihood :: [Experiment] -> SafeObjective
safeLikelihood expr syst = (try . evaluate) (likelihood expr syst)

-- | >>> testSolve exampleSystem [0.7,0.12,0.14,0.16,0.18] 6 (0,0.25)
-- (6><5)
--  [                0.7,               0.12,               0.14,               0.16,                0.18
--  , 1.1167718442535226, 0.5288004914151249, 0.9744853978475282, 1.0888238837276851, 0.40157122204470663
--  , 0.9667747407434063,  0.827179819885076, 0.9938518679839954, 1.0925434854072784,  0.8240710601831301
--  , 0.8394130067100113, 0.8627637078683361, 0.9980826732970266, 0.9920051282448126,  0.9522071825140638
--  , 0.7765858895457368, 0.8274811279700081,  0.999411143907833,  0.943541476235036,  0.9572057285536645
--  , 0.7499481576778694, 0.7905920430606405, 0.9998291350909083, 0.9281457408781921,  0.9424146353794791 ]
testSolve :: SSystem Double -> [Double] -> Int -> (Double,Double) -> Matrix Double
testSolve s t n i = solve s t (linspace n i)
