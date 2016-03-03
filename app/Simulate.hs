{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Arrow       ((***))
import           Data.Binary
import           Data.Map.Strict     (Map, fromList, lookup)
import           Data.Maybe          (fromMaybe)
import           Data.Text           (break, pack, split, tail, unpack)
import           Data.Text.Read      (double)
import           Prelude             hiding (FilePath, break, lines, lookup,
                                      tail, words)
import           Solver
import           SSystem
import           Turtle

parseVars :: Text -> Either String (Map String Double)
parseVars = fmap fromList . traverse parsePair . split (','==) where
  parsePair = sequenceA . (unpack *** fmap fst . double) . fmap tail . break (' '==)

sureLookup :: (Ord k, Show k, Show a) => k -> Map k a -> a
sureLookup k m = fromMaybe (error $ "Key " ++ show k ++ " not found in map: " ++ show m) (lookup k m)

main :: IO ()
main = stdout $ do
  file <- options "Simulation" (argPath "Path" "Path to serialized model")
  model <- liftIO $ decodeFile (unpack $ format fp file)
  dict <- either (die . pack) pure . parseVars =<< stdin
  runSolver $ withParams (`sureLookup` dict) model
