{-# LANGUAGE OverloadedStrings #-}

module CosmoABC
  ( InputFile(..)
  , cosmoABCSource
  )where

import           Data.Text (Text)
import qualified Data.Text as Text
import           SSystem

class CosmoABCCompat a where
  cosmoABCSource :: a -> Text

data InputFile = InputFile { particles :: Int
                           , mini      :: Int
                           , delta     :: Double
                           , screen    :: Bool
                           , cores     :: Int
                           , quantTshl :: Double
                           , splitOut  :: Int
                           , params    :: [Parameter]
                           }

instance CosmoABCCompat InputFile where
  cosmoABCSource (InputFile m i d s c q p ps) = flip Text.append " #" $ delimit $
    [ "file_root = SSystem"
    , "path_to_obs = None"
    , Text.append "param_to_fit = " pnames
    , Text.append "param_to_sim = " pnames
    , Text.append "prior_func = "   (Text.unwords $ map (const "flat_prior") ps)
    , Text.append "M = "            (Text.pack $ show m)
    , Text.append "Mini = "         (Text.pack $ show i)
    , Text.append "delta = "        (Text.pack $ show d)
    , Text.append "screen = "       (if s then "1" else "0")
    , Text.append "ncores = "       (Text.pack $ show c)
    , Text.append "qthreshold = "   (Text.pack $ show q)
    , Text.append "split_output = " (Text.pack $ show p)
    , "simulation_func = my_simulation"
    , "distance_func = my_distance"
    ] ++ (ps >>= \(Param mx mn sm n) -> map (Text.append (Text.pack n))
    [ "_prior_par_name = pmin pmax"
    , Text.unwords ["_prior_par_val = ", Text.pack $ show mn, Text.pack $ show mx]
    , Text.unwords ["_lim = ", Text.pack $ show mn, Text.pack $ show mx]
    , Text.append " = " (Text.pack $ show sm)])
    where
      delimit = Text.intercalate " #\n"
      pnames = (Text.unwords . map (Text.pack . nam)) ps
