{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Monad.State
import           Data.Text           (unpack)
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath)
import           Search
import           Solver
import           SSystem
import           Turtle              (FilePath, Parser, echo, format, fp, input,
                                      optPath, options, stdin, strict, view)
import           Utils

modelPath :: Parser (Maybe FilePath)
modelPath = optional $ optPath "Model" 'm' "Path to model. Reads from stdin otherwise"

simData :: Parser FilePath
simData = optPath "Data" 'd' "Path to simulation data"

main :: IO ()
main = view $ do
  (d,l,config) <- options "SSystem" ((,,) <$> simData
                                          <*> modelPath
                                          <*> simOptions)
  modelCode <- maybe stdin (strict . input) l
  simulation <- (toDie . parseOut) =<< (strict . input) d
  let desc = maybe "SSystem code from stdin" (unpack . format fp) l
  model <- toDie (parseSystem desc modelCode)
  let simulator = simMemo (fmap config . toDie . withParams model)
  final <- runMemo $ search simulator simulation (getParams model)
  echo "Final exponents:"
  pure final

