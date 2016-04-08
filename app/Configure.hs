{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.State
import           Data.Foldable
import           Data.Function
import           Data.List           (uncons)
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import           Data.Text           (pack, unpack)
import qualified Data.Text           as Text
import           Data.Text.Read
import           Data.Traversable
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath)
import           Solver
import           SSystem
import           Turtle              (FilePath, Parser, Shell, Text, die, echo,
                                      format, fp, input, optDouble, optInt,
                                      optPath, options, stdin, strict, view)
import           Utils

simOptions :: Parser (SSystem Double -> Simulation)
simOptions = Simulation <$> optDouble "Start" 's' "Start time for simulation"
                        <*> optDouble "Step" 'z' "Step size"
                        <*> optInt "Steps" 'n' "Number of steps"
                        <*> optDouble "AbsTol" 'a' "Absolute tolerance"
                        <*> optDouble "RelTol" 'r' "Relative tolerance"

modelPath :: Parser (Maybe FilePath)
modelPath = optional $ optPath "Model" 'm' "Path to model. Reads from stdin otherwise"

simData :: Parser FilePath
simData = optPath "Data" 'd' "Path to simulation data"

main :: IO ()
main = view $ do
  (d,l,config) <- options "SSystem" ((,,) <$> simData <*> modelPath <*> simOptions)
  modelCode <- maybe stdin (strict . input) l -- Reads model code from a file, if it was given, otherwise from stdin
  simulation <- parseOut <$> (strict . input) d
  let parsed = parseSystem (maybe "SSystem code from stdin" (Text.unpack . format fp) l) modelCode -- Parses model code
  model <- either (die . pack) pure parsed -- Converts Either ParseError Configurable to IO Configurable (with errors)
  let cmpFnc = cmpMemo (config . withParams model) simulation
  final <- evalMemo (minByM cmpFnc (lattice (getParams model)))
  echo "Final exponents:"
  pure final

memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
memo f x = gets (M.lookup x) >>= maybe new pure where
  new = do
    y <- lift (f x)
    modify (M.insert x y)
    pure y

evalMemo :: Monad m => StateT (M.Map a b) m c -> m c
evalMemo = flip evalStateT M.empty

simMemo :: ([Double] -> Simulation)
        -> [Double]
        -> StateT (Map [Double] [[Double]]) Shell [[Double]]
simMemo model = memo (fmap parseOut . runSolver . model)

parseOut :: Text -> [[Double]]
parseOut s = right ((const . error . unpack) s) . (traverse.traverse) (fmap fst . double) . map Text.words . Text.lines $ s

cmpMemo :: ([Double] -> Simulation)
        -> [[Double]]
        -> [Double]
        -> [Double]
        -> StateT (Map [Double] [[Double]]) Shell Ordering
cmpMemo m s = flip ((liftA2 . comparing) (mse (concat s))) `on` (fmap concat . simMemo m)

getParams :: SSystem NumLearn -> [[Double]]
getParams = toList . getNumLearn <=< toList

withParams :: SSystem NumLearn -> [Double] -> SSystem Double
withParams = evalState . traverse f  where
  f = either pure ((const . state) (fromMaybe err . uncons)) . getNumLearn
  err = error "Parameters and ssystem unmatched"
