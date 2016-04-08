{-# LANGUAGE OverloadedStrings #-}

module Solver where

import           Control.Monad.State
import           Data.Maybe
import           Data.Text           (pack)
import           Prelude             hiding (FilePath)
import           SSystem
import           Turtle              (Shell, Text, empty, format, fp, inproc,
                                      mktempdir, mktempfile, output, procs,
                                      using)
import           Utils

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
    f :: (Num a, Eq a, Show a) => STerm a -> State ([String],[ShowS]) ShowS
    f (STerm pf nf pe ne iv) = do
      (x:xs,vs) <- get
      put (xs,shows iv:vs)
      pure $ showString "diff(" . showString x . showString ", t) = " . showZ pf . p pe . showN nf . p ne where
          p = interc " * " . catMaybes . zipWith showO uniqNames
          showZ 0 s = s
          showZ n s = show n ++ " * " ++ s
          showO _ 0 = Nothing
          showO v 1 = Just (showString v)
          showO v n = Just (showString v . showString " ^ " . shows n)
          showN 0 s = s
          showN 1 s = " - " ++ s
          showN n s = " - " ++ show n ++ " * " ++ s
    g (l,(_,v)) = showString "initial_values=" (interc "," v (";\n" ++ interc ";\n" l ""))
    interc _ [] s = s
    interc c (x:xs) s = x $ foldr (\e a -> c ++ e a) s xs

data Simulation = Simulation { startTime :: Double
                             , stepSize  :: Double
                             , nSteps    :: Int
                             , absTol    :: Double
                             , relTol    :: Double
                             , system    :: SSystem Double
                             }

instance TaylorCompat Simulation where
  taylorSource (Simulation st ss ns at rt sy) = taylorSource sy ++ clines
                                                       [ "start_time=" ++ show st
                                                       , "step_size=" ++ show ss
                                                       , "number_of_steps=" ++ show ns
                                                       , "absolute_error_tolerance=" ++ show at
                                                       , "relative_error_tolerance=" ++ show rt]
