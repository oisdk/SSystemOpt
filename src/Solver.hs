{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solver
       ( simMemo
       , runMemo
       , parseOut
       , Simulation(..)
       , simOptions
       , withParams
       ) where

import           Control.Lens        hiding (strict)
import           Control.Monad.State
import           Data.Map.Strict     (Map)
import           Data.Maybe
import           Data.Text           (append, concat, intercalate, pack)
import qualified Data.Text           as Text
import           Data.Text.Read      (double)
import           Prelude             hiding (FilePath, concat)
import           Data.Square
import           SSystem
import           Turtle              (Parser, Shell, Text, echo, empty, format,
                                      fp, inproc, mktempdir, mktempfile,
                                      optDouble, optInt, output, procs, repr,
                                      strict, using)
import           Utils
-- | A typeclass for types with a representation in Taylor source code
class TaylorCompat a where taylorDecls :: a -> [Text]

taylorSource :: TaylorCompat a => a -> Text
taylorSource = concat . map (`append` ";\n") . taylorDecls

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation -> Shell Text
setup = inproc "taylor" ["-sqrt", "-step", "0", "-main"]
      . pure
      . taylorSource
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
  taylorDecls s = initials s  : derivs s where
    initials (SSystem _ t) =
      "initial_values=" `append` intercalate ", " (repr._initial <$> t)
    derivs (SSystem sq t) = imap f (zip uniqNames t) where
      f i (c, STerm post negt _ _) =
        concat ["diff(", pack c, ", t) = ", showOde post negt] where
          showOde 0 0 = "0"
          showOde 0 n = " - " `append` showSide n (side _2)
          showOde n 0 = showSide n (side _1)
          showOde n m = concat [showSide n (side _1), " - ", showSide m (side _2)]
          showSide 1 [] = "1"
          showSide 1 xs = intercalate " * " xs
          showSide n l = repr n `append` prepToAll " * " l
          side l = catMaybes $ zipWith expshow uniqNames (evals l)
          evals l = [ sq ^?! ix (i,j) . l | j <- [0..(_squareSize sq - 1)]]
          expshow _ 0 = Nothing
          expshow n 1 = Just $ pack n
          expshow n e = Just $ concat [pack n, " ^ ", repr e]

prepToAll :: Text -> [Text] -> Text
prepToAll _ [] = ""
prepToAll y (x:xs) = intercalate y (append y x : xs)


-- | The full information needed to run a Taylor simulation
data Simulation =
  Simulation { startTime :: Double
             , stepSize  :: Double
             , nSteps    :: Int
             , absTol    :: Double
             , relTol    :: Double
             , system    :: SSystem Double}


instance TaylorCompat Simulation where
  taylorDecls (Simulation st ss ns abt rlt sy) =
    [ "start_time="               `append` repr st
    , "step_size="                `append` repr ss
    , "number_of_steps="          `append` repr ns
    , "absolute_error_tolerance=" `append` repr abt
    , "relative_error_tolerance=" `append` repr rlt]
    ++ taylorDecls sy

simOptions :: Parser (SSystem Double -> Simulation)
simOptions = Simulation <$> optDouble "Start"  's' "Start time for simulation"
                        <*> optDouble "Step"   'z' "Step size"
                        <*> optInt    "Steps"  'n' "Number of steps"
                        <*> optDouble "AbsTol" 'a' "Absolute tolerance"
                        <*> optDouble "RelTol" 'r' "Relative tolerance"

-- | Memoizes a function using the state transformer monad
memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
-- memo f x = maybe ((at x <?=) =<< lift (f x)) pure =<< use (at x)
memo f x = do
  old <- use (at x)
  case old of
    Just y -> pure y
    Nothing -> do
      y <- lift (f x)
      at x <?= y

-- | Parses the output from a Taylor simulation
parseOut :: Text -> Either String [[Double]]
parseOut = (traverse.traverse) (fmap fst . double)
         . map Text.words
         . Text.lines

simMemo :: ([Double] -> Shell Simulation)
        -> [Double]
        -> StateT (Map [Double] (Maybe [[Double]])) Shell (Maybe [[Double]])
simMemo model = memo (\ns -> (echo . repr) ns *> model ns >>= runSolver)

runMemo :: (Monoid m, Monad f)
        => StateT m f a -> f a
runMemo = flip evalStateT mempty

withParams :: SSystem NumLearn -> [Double] -> Either String (SSystem Double)
withParams s =
  maybe (Left err) Right .
    evalSource (traverse (either pure (const pop)) s) where
      err = "Mismatched params"
