{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Solver where

import           Data.List (intercalate, intersperse)
import           Data.Text (pack)
import           Prelude   hiding (FilePath)
import           SSystem
import           Turtle
import           Utils
import Control.Arrow (second)
import Control.Monad.State

class TaylorCompat a where
  taylorSource :: a -> String

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation-> Shell Text
setup = inproc "taylor" ["-sqrt", "-step", "0", "-main"] . pure . pack . taylorSource

-- | Given a configuration, prints a the output to the stdout,
-- after compiling and running.
runSolver :: Simulation -> Shell Text
runSolver c = do
  dir <- using (mktempdir "/tmp" "ssystems")
  cpath <- using (mktempfile dir "solver.c")
  output cpath (setup c)
  opath <- using (mktempfile dir "solver.o")
  let ostr = format fp opath
  procs "gcc" ["-O3", "-o", ostr, format fp cpath] empty
  inproc ostr [] empty


instance (Num a, Eq a, Show a) => TaylorCompat (SSystem a) where
  taylorSource = g . flip runState (uniqNames, []) . traverse f . getSSystem where
    f :: (Num a, Eq a, Show a) => STerm a -> State ([String],[a]) String
    f (STerm pf nf pe ne iv) = do
      (x:xs) <- gets fst
      modify (second (iv:))
      pure $ "diff(" ++ x ++ ", t) = " ++ showO pf ++ p pe ++ showN nf ++ p ne where
          p = intercalate " * " . zipWith ((. showO) . (++)) uniqNames
          showZ 0 = ""
          showZ n = show n ++ " * "
          showO 1 = ""
          showO n = " ^ " ++ show n
          showN 0 = ""
          showN n = " - " ++ show n ++ " * "
    g (l,(_,v)) = clines $ ("initial_values=" ++ interpose ',' (map show v)) : l

data Simulation = Simulation { startTime :: Double
                             , stepSize :: Double
                             , nSteps :: Int
                             , absTol :: Double
                             , relTol :: Double
                             , system :: SSystem Double
                             }
instance TaylorCompat Simulation where
  taylorSource (Simulation st ss ns at rt sy) = clines [ taylorSource sy
                                                       , "start_time=" ++ show st
                                                       , "step_size=" ++ show ss
                                                       , "number_of_steps=" ++ show ns
                                                       , "absolute_error_tolerance=" ++ show at]
  
interpose :: a -> [[a]] -> [a]
interpose _ [] = []
interpose _ [x] = x
interpose y (x:xs) = x ++ foldr (\e a -> y : e ++ a) [] xs
