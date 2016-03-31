{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.State
import           Data.Foldable       (toList)
import           Data.Function
import           Data.Map            (Map)
import qualified Data.Map            as M
import           Data.Maybe
import           Data.Ord
import           Data.Text           (unpack)
import qualified Data.Text           as Text
import           Data.Text.Read
import           Parse               (parseSystem)
import           Prelude             hiding (FilePath, concat)
import           Solver
import           SSystem
import           Turtle
import           Utils
import           Zipper

optOptions :: Parser (Maybe FilePath, Double, Double)
optOptions = (,,) <$> optional (optPath "Path" 'p' "Path to the model file. Reads from stdin otherwise.")
                  <*> optDouble "Min" 'i' "Minimum value for exponents"
                  <*> optDouble "Max" 'a' "Maximum value for exponents"

main :: IO ()
main = view $ do
  (l,i,a) <- options "SSystem" optOptions
  modelCode <- maybe stdin (strict . input) l -- Reads model code from a file, if it was given, otherwise from stdin
  let parsed = parseSystem (maybe "SSystem code from stdin" (Text.unpack . format fp) l) modelCode -- Parses model code
  let bounds = Bounds i a (0.5+)
  model <- either (die . repr) pure parsed -- Converts Either ParseError Configurable to IO Configurable (with errors)
  simVal <- parseOut <$> strict (runSolver (simulationVal model))
  let init = (fromList . (fmap . const . minB) bounds . justParams) model
  let cmpFnc = cmpMemo model simVal
  final <- toList <$> evalMemo (searchM cmpFnc bounds init)
  echo "Final exponents:"
  pure final

name :: Zipper Double -> Map String Double
name = M.fromList . zipF uniqNames

sureLookup :: (Ord k, Show k, Show a) => k -> Map k a -> a
sureLookup k m = fromMaybe (error $ "Key " ++ show k ++ " not found in map: " ++ show m) (M.lookup k m)

memo :: (Monad m, Ord a) => (a -> m b) -> a -> StateT (Map a b) m b
memo f x = gets (M.lookup x) >>= maybe new pure where
  new = do
    y <- lift (f x)
    modify (M.insert x y)
    pure y

evalMemo :: Monad m => StateT (M.Map a b) m c -> m c
evalMemo = flip evalStateT M.empty

simMemo :: Configurable (Either Double Parameter)
        -> Zipper Double
        -> StateT (M.Map (Zipper Double) [Double]) Shell [Double]
simMemo model = memo f where
  f z = do
    (echo . repr . toList) z
    out <- strict (runSolver (withParams (`sureLookup` name z) model))
    pure (parseOut out)

parseOut :: Text -> [Double]
parseOut s = either (\_ -> error (unpack s)) id . traverse (fmap fst . double) . Text.words $ s

cmpMemo :: Configurable (Either Double Parameter)
        -> [Double]
        -> Zipper Double
        -> Zipper Double
        -> StateT (M.Map (Zipper Double) [Double]) Shell Ordering
cmpMemo m s = flip ((liftA2 . comparing) (mse s . toList)) `on` simMemo m
