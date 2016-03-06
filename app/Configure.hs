{-# LANGUAGE OverloadedStrings #-}

module Main where

import           CosmoABC
import           Data.Serialize
import           Data.Text   (unpack, concat)
import           Parse       (parseSystem)
import           Prelude     hiding (FilePath, concat)
import           SSystem
import           Turtle
import qualified Data.ByteString as ByteString

optOptions :: Parser (Int, Int, Double, Double, Maybe FilePath)
optOptions = (,,,,) <$> optInt "Particles" 'p' "Number of particles in each particle system"
                    <*> optInt "Draws" 'm' "Number of draws for the first particle system"
                    <*> optDouble "Delta" 'd' "Convergence criteria"
                    <*> optDouble "QThreshold" 'q' "Quantile in distance threshold"
                    <*> optional (optPath "Model" 'm' "Path to the model file. Reads from stdin otherwise.")

main :: IO ()
main = sh $ do
  (p,m,c,q,l) <- options "SSystem" optOptions -- Parses cosmoabc's options
  modelCode <- maybe stdin (strict . input) l -- Reads model code from a file, if it was given, otherwise from stdin
  cd "/Users/oisinkidney/Desktop/Code/SSystemOpt/Working"
  let mdl = "/Users/oisinkidney/Desktop/Code/SSystemOpt/Working/model"
  exists <- testpath mdl
  when exists (rm mdl)
  touch mdl
  let parsed = parseSystem (maybe "SSystem code from stdin" (unpack . format fp) l) modelCode -- Parses model code
  model <- either (die . repr) pure parsed -- Converts Either ParseError Configurable to IO Configurable (with errors)
  liftIO $ ByteString.writeFile (unpack $ format fp mdl) (encode model)-- Encodes model
  let cfg = "/Users/oisinkidney/Desktop/Code/SSystemOpt/Working/config"
  cfgexists <- testpath cfg
  when cfgexists (rm cfg)
  touch cfg
  let cosmoConfig = InputFile { particles = p
                              , mini      = m
                              , delta     = c
                              , screen    = True
                              , cores     = 1
                              , quantTshl = q
                              , splitOut  = 1
                              , params    = justParams model
                              }
  output cfg (pure $ cosmoABCSource cosmoConfig) -- Outputs cosmoABC's config file
  let pyf = "/Users/oisinkidney/Desktop/Code/SSystemOpt/Working/functions.py"
  pyexists <- testpath pyf
  when pyexists (rm pyf)
  output pyf (pure $ pythonProgram (format fp mdl))

pythonProgram :: Text -> Text
pythonProgram modelLoc = concat ["\
\from scipy.stats import uniform\n\
\import numpy as np\n\
\import subprocess\n\
\import sys\n\
\\n\
\def my_simulation(v):\n\
\\n\
\    exeloc = '/Users/oisinkidney/Desktop/Code/SSystemOpt/.stack-work/install/x86_64-osx/lts-5.1/7.10.3/bin/simulate'\n\
\    proc = subprocess.Popen([exeloc, \"", modelLoc, "\"], stdin=subprocess.PIPE, stdout=subprocess.PIPE)\n\
\    outs, _ = proc.communicate(','.join(name + ' ' + str(val) for name, val in v.items()) + '\\n')\n\
\    res = np.atleast_2d([[float(n) for n in row.split()] for row in outs.splitlines()])\n\
\    return res\n\
\\n\
\\n\
\def my_prior(par, func=False):\n\
\\n\
\    gap = par['max'] - par['min']\n\
\    pdf = uniform(loc=par['min'], scale=gap)\n\
\    return pdf if func else pdf.rvs()\n\
\\n\
\\n\
\def my_distance(d2, p):\n\
\\n\
\    mean_obs = np.mean(p['dataset1'])\n\
\    std_obs = np.std(p['dataset1'])\n\
\\n\
\    gmean = abs((mean_obs - np.mean(d2)) / mean_obs)\n\
\    gstd = abs((std_obs - np.std(d2)) / std_obs)\n\
\\n\
\    rho = gmean + gstd\n\
\\n\
\    return np.atleast_1d(rho)"]
