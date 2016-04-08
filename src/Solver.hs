{-# LANGUAGE OverloadedStrings #-}

module Solver
       ( search
       , parseOut
       , Simulation(..)
       , simOptions
       ) where

import           Control.Applicative (liftA2)
import           Control.Monad.State
import           Data.Function       (on)
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Ord            (comparing)
import           Data.Text           (pack)
import qualified Data.Text           as Text
import           Data.Text.Read      (double)
import           Prelude             hiding (FilePath)
import           Search
import           SSystem
import           Turtle              (Parser, Shell, Text, empty, format, fp,
                                      inproc, mktempdir, mktempfile, optDouble,
                                      optInt, output, procs, using)
import           Utils

class TaylorCompat a where
  taylorSource :: a -> String

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation -> Shell Text
setup = inproc "taylor" ["-sqrt", "-step", "0", "-main"] . pure . pack . taylorSource

-- | Given a configuration, prints a the output to the stdout,
-- after compiling and running.
runSolver :: Simulation -> Shell [[Double]]
runSolver c = do
  dir <- using (mktempdir "/tmp" "ssystems")
  cpath <- using (mktempfile dir "solver.c")
  output cpath (setup c)
  opath <- using (mktempfile dir "solver.o")
  let ostr = format fp opath
  procs "gcc" ["-O3", "-o", ostr, format fp cpath] empty
  out <- inproc ostr [] empty
  parseOut out


instance (Num a, Eq a, Show a) => TaylorCompat (SSystem a) where
  taylorSource = g . flip runState (uniqNames, []) . traverse f . getSSystem where
    f :: (Num a, Eq a, Show a) => STerm a -> State ([String],[ShowS]) ShowS
    f (STerm pf nf pe ne iv) = do
      (x:xs,vs) <- get
      put (xs,shows iv:vs)
      pure (rest pf pe nf ne x)
    rest pf pe nf ne x s = "diff(" ++ x ++ ", t) = " ++ (showZ pf . p pe . showN nf . p ne) s
    p :: (Num a, Eq a, Show a) => [a] -> ShowS
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

simOptions :: Parser (SSystem Double -> Simulation)
simOptions = Simulation <$> optDouble "Start" 's' "Start time for simulation"
                        <*> optDouble "Step" 'z' "Step size"
                        <*> optInt "Steps" 'n' "Number of steps"
                        <*> optDouble "AbsTol" 'a' "Absolute tolerance"
                        <*> optDouble "RelTol" 'r' "Relative tolerance"

memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
memo f x = gets (M.lookup x) >>= maybe new pure where
  new = do
    y <- lift (f x)
    modify (M.insert x y)
    pure y

parseOut :: Text -> Shell [[Double]]
parseOut = toDie
         . (traverse.traverse) (fmap fst . double)
         . map Text.words
         . Text.lines

simMemo :: ([Double] -> Shell Simulation)
        -> [Double]
        -> StateT (Map [Double] [[Double]]) Shell [[Double]]
simMemo model = memo (runSolver <=< model)

search :: ([Double] -> Shell Simulation)
       -> [[Double]]
       -> [[Double]]
       -> Shell (Maybe [Double])
search m s = flip evalStateT M.empty . minByM cmpFnc . lattice where
  cmpFnc = flip ((liftA2 . comparing) (rmse s)) `on` simMemo m
