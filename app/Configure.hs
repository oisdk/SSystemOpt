{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.Foldable       (toList)
import           Data.Function
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import qualified Data.Text           as Text
import           Data.Text.Read
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath, concat)
import           Solver
import           SSystem
import           Turtle
import           Utils               hiding (evalState)
import           Zipper

optOptions :: Parser (Maybe FilePath)
optOptions = optional (optPath "Model" 'm' "Path to the model file. Reads from stdin otherwise.")

main :: IO ()
main = view $ do
  l <- options "SSystem" (optional (argPath "Path" "Path to the model file. Reads from stdin otherwise."))
  modelCode <- maybe stdin (strict . input) l -- Reads model code from a file, if it was given, otherwise from stdin
  let parsed = parseSystem (maybe "SSystem code from stdin" (Text.unpack . format fp) l) modelCode -- Parses model code
  model <- either (die . repr) pure parsed -- Converts Either ParseError Configurable to IO Configurable (with errors)
  simVal <- parseOut <$> strict (runSolver (simulationVal model))
  let init = (fromList . fmap (const minBound) . justParams) model
  let cmpFnc = cmpMemo model simVal
  (map getPE . toList) <$> evalStateT (searchM cmpFnc init) M.empty

name :: Zipper PosExp -> Map String Double
name = M.fromList . zip uniqNames . foldr (:) [] . fmap getPE

sureLookup :: (Ord k, Show k, Show a) => k -> Map k a -> a
sureLookup k m = fromMaybe (error $ "Key " ++ show k ++ " not found in map: " ++ show m) (M.lookup k m)

memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
memo f x = gets (M.lookup x) >>= maybe new pure where
  new = do
    y <- lift (f x)
    modify (M.insert x y)
    pure y

evalMemo :: Monad m => StateT (M.Map a b) m b -> m b
evalMemo = flip evalStateT M.empty

simMemo :: Configurable (Either Double Parameter)
        -> Zipper PosExp
        -> StateT (M.Map (Zipper PosExp) [Double]) Shell [Double]
simMemo model = memo (\z -> parseOut <$> strict (runSolver (withParams (`sureLookup` name z) model)))

parseOut :: Text -> [Double]
parseOut = either error id . traverse (fmap fst . double) . Text.words

cmpMemo :: Configurable (Either Double Parameter)
        -> [Double]
        -> Zipper PosExp
        -> Zipper PosExp
        -> StateT (M.Map (Zipper PosExp) [Double]) Shell Ordering
cmpMemo m s = flip ((liftA2 . comparing) (mse s . toList)) `on` simMemo m
