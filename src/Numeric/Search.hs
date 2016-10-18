{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TemplateHaskell  #-}


module Numeric.Search where

import           Control.Lens
import           Control.Monad.State.Strict
import           Data.Utils
import           Numeric.Solver
import           Numeric.SSystem
import           Text.Parse.ODEBench
import Data.Foldable
import Control.Monad.Reader
import Data.Maybe
import Numeric.GSL
import Data.Traversable
import System.IO


data SearchEnvironment = SearchEnvironment
  { _paramBounds :: SSystem Bounds
  , _objectiveFunc :: SafeObjective }

makeLenses ''SearchEnvironment

data SearchState = SearchState
  { _counter :: Int
  , _curSystem :: SSystem Double
  , _bestScore :: Double }

makeLenses ''SearchState

type Search a =
  forall m. (MonadState SearchState m
            ,MonadReader SearchEnvironment m
            ,MonadIO m
            ) => m a


inBounds :: Double -> Bounds -> Bool
inBounds n Bounds {..} = n <= upper && n >= lower

tryAdd :: Double
       -> (forall a. Traversal' (SSystem a) a) -- ^ Parameter to try and add
       -> Search ()
tryAdd fac param = do
  syst <- use curSystem
  vary syst (\val -> [ val `o` (i*fac) | i <- [0.2, 0.5, 1], o <- [(+),(-)] ]) param >>= \case
    Nothing -> pure ()
    Just (best,bscore) -> do
      baseScore <- use bestScore
      when (baseScore < bscore) $ do
        counter += 1
        curSystem .= best
        bestScore .= bscore
        printState

isInBound :: MonadReader SearchEnvironment m
          => Traversal' (SSystem Bounds) Bounds
          -> Double
          -> m Bool
isInBound param =
  views (pre $ paramBounds.param) . maybe False . inBounds

vary :: (MonadReader SearchEnvironment m, MonadIO m)
     => SSystem Double
     -> (Double -> [Double])
     -> (forall a. Traversal' (SSystem a) a)
     -> m (Maybe (SSystem Double, Double))
vary syst fac param = do
  obj <- view objectiveFunc
  fmap join $ for (syst ^? param) $ \val -> do
    newParams <- filterM (isInBound param) (fac val)
    let variants = [ param .~ newv $ syst | newv <- newParams ]
    fmap (maximumOn snd . catMaybes) $ for variants $ \ss ->
      liftIO (obj ss) >>= \case
        Left e -> Nothing <$ liftIO (print e)
        Right score -> pure $ Just (ss,score)

search :: Search ()
search = loop where
  loop = do
    prev <- use bestScore
    oneRound
    changed <- uses bestScore (prev/=)
    when changed loop

printState :: (MonadState SearchState m, MonadIO m) => m ()
printState = do
  liftIO . print =<< use curSystem
  liftIO . print =<< use bestScore
  liftIO (hFlush stdout)

oneRound :: Search ()
oneRound = do
  inds <- uses (curSystem.size) (enumFromTo 0 . pred)
  syst <- use curSystem
  for_ inds $ \i -> do
    tryAdd 5 (alphal i)
    tryAdd 5 (betal  i)
  nexts <- for inds $ \i -> for inds $ \j -> do
      printState
      pos <- vary syst (\e -> [e-1,e+1]) (posExpl i j)
      neg <- vary syst (\e -> [e-1,e+1]) (negExpl i j)
      printState
      pure (catMaybes [pos,neg])
  for_ (maximumOn snd . concat . concat $ nexts) $ \(best,bscore) -> do
    curSystem .= best
    bestScore .= bscore
  printState

basicFind :: String -> IO ()
basicFind f = do
  prob <- fromFile f
  let obj = safeLikelihood (experiments prob)
  let bnds = fromParams (space prob)
  let syst = fromParams (initialSoln prob)
  flip evalStateT (SearchState 0 syst (likelihood (experiments prob) syst)) $
       runReaderT search (SearchEnvironment bnds obj)

exampleFind :: IO ()
exampleFind = do
  setErrorHandlerOff
  basicFind "/Users/oisinkidney/Developer/NatSSystemOpt/data/ss_5genes.txt"
