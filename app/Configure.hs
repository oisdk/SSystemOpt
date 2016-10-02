{-# LANGUAGE OverloadedStrings #-}

module Main where

-- import qualified Data.Text.Lazy.IO         as T
import           Experiments
import           Filesystem.Path.CurrentOS
-- import           SBML
import           Search
import           System.Exit
-- import           Text.Taggy.Renderer
import           Text.Trifecta.Parser
import           Turtle                    (optPath, options)
import Control.Monad
import Solver
import Turtle.Shell
import Data.Maybe
import Statistics.Regression
import Data.Ord
import Data.Foldable
import qualified Data.Vector.Unboxed as Vector


main :: IO ()
main = view $ do
  p <- options "SSystem" $ optPath "Problem" 'p' "Path to problem file"
  (ss,df) <- maybe (liftIO exitFailure) pure =<< parseFromFile problem (encodeString p)
  stepSize <- maybe (liftIO exitFailure) pure (experimentStepSize df)
  let sttime = stopTime df
  let nstps = nsteps df
  let preds = toPredictors df
  ress <- forM (versions ss) $ \sstm -> do
    let sim = Simulation 0 stepSize (round nstps) 0.001 0.001 sstm
    resp <- responders sim
    case resp of
      Nothing -> pure Nothing
      Just resp' -> do
        let (_,rsq) = olsRegress preds resp'
        liftIO $ print rsq
        pure (Just (sstm, rsq))
  let noMaybs = mapMaybe id ress
  let best = maximumOn snd noMaybs
  liftIO $ print best
  -- liftIO $ print ress
  -- liftIO $ print df
  -- liftIO $ print (toPredictors df)
  -- T.writeFile (encodeString c) (toExpFormat df)
  -- T.writeFile (encodeString s) ((render . toSBML) ss)


maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (comparing f)
