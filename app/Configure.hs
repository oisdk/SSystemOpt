{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens        hiding (strict, view)
import           Control.Monad.State
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text.Read      (double)
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath)
import           SBML
import           Search
import           Solver
import           SSystem
import           Turtle              (FilePath, Parser, Shell, die, echo, input,
                                      optPath, options, strict, view)
import           Turtle.Prelude      (readline)
import           Utils

modelPath :: Parser FilePath
modelPath = optPath "Model" 'm' "Path to model"

simData :: Parser (Maybe FilePath)
simData = optional $ optPath "Data" 'd' "Path to simulation data"

main :: IO ()
main = view $ do
  (d,l,(stt,sts,stn,atl,rtl)) <- options "SSystem" ((,,) <$> simData
                                          <*> modelPath
                                          <*> simOptions)
  let config = Simulation stt sts stn atl rtl
  modelCode <- (strict . input) l
  model <- toDie (parseSystem modelCode)
  let simulationM = withParams model
  let simulator = simMemo (fmap config . toDie . simulationM)
  simulation <- case d of
    Just x -> do
      y <- strict . input $ x
      liftIO (print y)
      (toDie . parseOut) y
    Nothing -> do
      simVersion <- fillModel model
      maybe (die "Simulation version did not produce usable model") pure =<< runSolver (config simVersion)
  echo . toExpFormat $ Experiment
    atl "net1" sts
    [  VariableHistory (Text.pack n) (map (!!i) simulation)
    | (n,i) <- zip (model ^.. terms . each . name) [0..]]
  final <- runMemo $ search simulator simulation (model ^.. folded._Right)
  echo "Final exponents:"
  pure final

fillModel :: SSystem NumLearn -> Shell (SSystem Double)
fillModel = traverse ((either pure . const) (fmap fst $ toDie =<< double . fromMaybe "end of input" <$> readline))
