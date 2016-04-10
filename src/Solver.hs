{-# LANGUAGE OverloadedStrings #-}

module Solver
       ( simMemo
       , runMemo
       , parseOut
       , Simulation(..)
       , simOptions
       ) where

import           Control.Monad.State
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Data.Text           (append, concat, intercalate, pack)
import qualified Data.Text           as Text
import           Data.Text.Read      (double)
import           Prelude             hiding (FilePath, concat)
import           SSystem
import           Turtle              (Parser, Shell, Text, empty, format, fp,
                                      inproc, mktempdir, mktempfile, optDouble,
                                      optInt, output, procs, repr, using, echo, strict)
import           Utils


-- | A typeclass for types with a representation in Taylor source code
class TaylorCompat a where
  taylorSource :: a -> Text

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation -> Shell Text
setup = inproc "taylor" ["-sqrt", "-step", "0", "-main"] . pure . taylorSource
-- setup s = do
--   let ts = taylorSource s
--   echo ts
--   (inproc "taylor" ["-sqrt", "-step", "0", "-main"] . pure) ts

-- | Given a configuration, prints a the output to the stdout,
-- after compiling and running.
runSolver :: Simulation -> Shell (Maybe [[Double]])
runSolver c = do
  dir <- using (mktempdir "/tmp" "ssystems")
  cpath <- using (mktempfile dir "solver.c")
  output cpath (setup c)
  opath <- using (mktempfile dir "solver.o")
  let ostr = format fp opath
  procs "gcc" ["-O3", "-o", ostr, format fp cpath] empty
  out <- strict $ inproc ostr [] empty
  pure $ eitherToMaybe (parseOut out)

instance (Num a, Eq a, Show a) => TaylorCompat (SSystem a) where
  taylorSource = g . flip runState (uniqNames, []) . traverse f . getSSystem where
    f :: (Num a, Eq a, Show a) => STerm a -> State ([String],[Text]) Text
    f (STerm pf nf pe ne iv) = do
      (x:xs,vs) <- get
      put (xs,repr iv:vs)
      pure (rest pf pe nf ne x)
    rest pf pe nf ne x = concat ["diff(", pack x, ", t) = ", showZ pf, p pe, showN nf, p ne]
    p :: (Num a, Eq a, Show a) => [a] -> Text
    p = concat . catMaybes . zipWith showO uniqNames
    showZ 0 = "0"
    showZ n = repr n
    showO _ 0 = Nothing
    showO v 1 = Just (" * " `append` pack v)
    showO v n = (Just . concat) [" * ", pack v, " ^ ", repr n]
    showN 0 = ""
    showN n = concat [" - ", repr n]
    g (l,(_,v)) = concat ["initial_values=", intercalate "," v, ";\n", intercalate ";\n" l] `append` ";"

-- | The full information needed to run a Taylor simulation
data Simulation = Simulation { startTime :: Double
                             , stepSize  :: Double
                             , nSteps    :: Int
                             , absTol    :: Double
                             , relTol    :: Double
                             , system    :: SSystem Double
                             }

instance TaylorCompat Simulation where
  taylorSource (Simulation st ss ns at rt sy) =
    intercalate ";\n" [ append "start_time=" (repr st)
                      , append "step_size=" (repr ss)
                      , append "number_of_steps=" (repr ns)
                      , append "absolute_error_tolerance=" (repr at)
                      , append "relative_error_tolerance=" (repr rt)
                      , taylorSource sy]

simOptions :: Parser (SSystem Double -> Simulation)
simOptions = Simulation <$> optDouble "Start" 's' "Start time for simulation"
                        <*> optDouble "Step" 'z' "Step size"
                        <*> optInt "Steps" 'n' "Number of steps"
                        <*> optDouble "AbsTol" 'a' "Absolute tolerance"
                        <*> optDouble "RelTol" 'r' "Relative tolerance"

-- | Memoizes a function using the state transformer monad
memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
memo f x = gets (M.lookup x) >>= maybe new pure where
  new = do
    y <- lift (f x)
    modify (M.insert x y)
    pure y

-- | Parses the output from a Taylor simulation
parseOut :: Text -> Either String [[Double]]
parseOut = (traverse.traverse) (fmap fst . double)
         . map Text.words
         . Text.lines

simMemo :: ([Double] -> Shell Simulation)
        -> [Double]
        -> StateT (Map [Double] (Maybe [[Double]])) Shell (Maybe [[Double]])
simMemo model = memo (\ns -> (echo . repr) ns >> model ns >>= runSolver)

runMemo :: (Monoid m, Monad f)
        => StateT m f a -> f a
runMemo = flip evalStateT mempty
