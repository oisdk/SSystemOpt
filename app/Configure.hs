{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Lens        hiding (strict, view)
import           Control.Monad.State
import           Data.Maybe
import           Data.Text           (unpack)
import           Data.Text.Read      (double)
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath)
import           Search
import           Solver
import           SSystem
import           Turtle              (FilePath, Parser, Shell, die, echo,
                                      format, fp, input, optPath, options,
                                      strict, view)
import           Turtle.Prelude      (readline)
import           Utils

modelPath :: Parser FilePath
modelPath = optPath "Model" 'm' "Path to model"

simData :: Parser (Maybe FilePath)
simData = optional $ optPath "Data" 'd' "Path to simulation data"

main :: IO ()
main = view $ do
  (d,l,config) <- options "SSystem" ((,,) <$> simData
                                          <*> modelPath
                                          <*> simOptions)
  modelCode <- (strict . input) l
  let desc = (unpack . format fp) l
  model <- toDie (parseSystem desc modelCode)
  let simulator = simMemo (fmap config . toDie . withParams model)
  simulation <- case d of
    Just x -> (toDie . parseOut) =<< (strict . input) x
    Nothing -> do
      simVersion <- fillModel model
      maybe (die "Simulation version did not produce usable model") pure =<< runSolver (config simVersion)
  final <- runMemo $ search simulator simulation (model ^.. folded._Right)
  echo "Final exponents:"
  pure final

fillModel :: SSystem NumLearn -> Shell (SSystem Double)
fillModel = traverse ((either pure . const) (fmap fst $ toDie =<< double . fromMaybe "end of input" <$> readline))
