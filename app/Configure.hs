{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.Lazy.IO    as T
import           Experiments
import           SBML
import           System.Exit
import           Text.Taggy.Renderer
import           Text.Trifecta.Parser
import           Turtle               (optPath, options)

main :: IO ()
main = do
  (p,s,c) <- options "SSystem" $ (,,) <$> optPath "Problem" 'p' "Path to problem file"
                                      <*> optPath "SBML-out" 's' "Path to sbml output"
                                      <*> optPath "Python-out" 'c' "Path to python output"
  (ss,df) <- maybe exitFailure pure =<< parseFromFile problem (show p)
  T.writeFile (show c) (toExpFormat df)
  T.writeFile (show s) ((render . toSBML . fmap Right) ss)
