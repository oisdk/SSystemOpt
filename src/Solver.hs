{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solver where

import           Control.Lens            hiding (strict)
import           Control.Monad.State
import           Data.Functor
import           Data.Map.Strict         (Map)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence           as Seq
import           Data.Text               (append, intercalate)
import qualified Data.Text               as Text
import           Data.Text.Read          (double)
import qualified Data.Vector.Unboxed     as Vec
import           GHC.Exts
import           Numeric.Expr
import           Prelude                 hiding (FilePath, concat)
import           SSystem
import           Statistics.Matrix.Types
import           Turtle                  (Parser, Shell, Text, echo, empty,
                                          format, fp, inproc, mktempdir,
                                          mktempfile, optDouble, optInt, output,
                                          procs, repr, strict, using)
import           Utils                   hiding (zipWith)
-- | A typeclass for types with a representation in Taylor source code
class TaylorCompat a where taylorDecls :: a -> [Text]

taylorSource :: TaylorCompat a => a -> Text
taylorSource = Text.concat . map (`append` ";\n") . taylorDecls

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation -> Shell Text
-- setup = inproc "taylor" ["-sqrt", "-step", "0", "-main"]
--       . pure
--       . taylorSource
setup s = do
  let ts = taylorSource s
  echo ts
  (inproc "taylor" ["-sqrt", "-step", "0", "-main"] . pure) ts

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
  either (pure . const Nothing) (\r -> echo out $> Just r) (parseOut out)

instance (Floating a, Eq a, Show a) => TaylorCompat (SSystem (VarExpr a)) where
  taylorDecls s = inits : imap eqnmake derivs where
    inits = "initial_values=" `append` intercalate ", " (s^..initials.each.to repr)
    derivs :: [VarExpr a]
    derivs = foldr (:) [] $ toEqns (Var . fromString . Seq.index varNames) s
    varNames = Seq.fromList (takeStream (size s) uniqNames)
    eqnmake i eqn = "diff(" <> fromString (Seq.index varNames i) <> ",t) = " <> repr eqn

-- | The full information needed to run a Taylor simulation
data Simulation =
  Simulation { startTime :: Double
             , stepSize  :: Double
             , nSteps    :: Int
             , absTol    :: Double
             , relTol    :: Double
             , system    :: SSystem (VarExpr Double)}


instance TaylorCompat Simulation where
  taylorDecls (Simulation st ss ns abt rlt sy) =
    [ "start_time="               `append` repr st
    , "step_size="                `append` repr ss
    , "number_of_steps="          `append` repr ns
    , "absolute_error_tolerance=" `append` repr abt
    , "relative_error_tolerance=" `append` repr rlt]
    ++ taylorDecls sy

simOptions :: Parser (Double, Double, Int, Double, Double)
simOptions = (,,,,) <$> optDouble "Start"  's' "Start time for simulation"
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

responders :: Simulation -> Shell (Maybe Vector)
responders = (fmap.fmap) (Vec.fromList . join) . runSolver

simMemo :: ([Double] -> Shell Simulation)
        -> [Double]
        -> StateT (Map [Double] (Maybe [[Double]])) Shell (Maybe [[Double]])
simMemo model = memo (\ns -> (echo . repr) ns *> model ns >>= runSolver)

runMemo :: (Monoid m, Monad f)
        => StateT m f a -> f a
runMemo = flip evalStateT mempty

withParams :: SSystem NumLearn -> [Double] -> Either String (SSystem (VarExpr Double))
withParams s =
    evalStateT (traverse (either pure (const pop')) s) where
      pop' = Lit <$> StateT (maybe err Right . uncons)
      err = Left "Mismatched params"
