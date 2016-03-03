{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Solver where

import           Data.List (intercalate)
import           Data.Text (pack)
import           Prelude   hiding (FilePath)
import           SSystem
import           Turtle
import           Utils

class TaylorCompat a where
  taylorSource :: a -> String

-- | Uses taylor to generate a solver (uncompiled, in c code)
-- for a given configuration
setup :: Simulation-> Shell Text
setup = inproc "taylor" ["-sqrt"] . pure . pack . taylorSource

-- | Given a configuration, prints a the output to the stdout,
-- after compiling and running.
runSolver :: Simulation -> Shell Text
runSolver c = do
  dir <- using (mktempdir "/tmp" "ssystems")
  cpath <- using (mktempfile dir "solver.c")
  output cpath (setup c)
  opath <- using (mktempfile dir "solver.o")
  let ostr = format fp opath
  procs "gcc" ["-O3", "-o", ostr, format fp cpath] empty
  inproc ostr [] empty

instance (Eq d, Num d, Show d) => TaylorCompat (Configurable d) where
  taylorSource (Configurable sy st sp ae re ss) =
    taylorSource sy ++ "\n\n" ++ clines [ "start_time="               ++ show st
                                        , "stop_time="                ++ show sp
                                        , "absolute_error_tolerance=" ++ show ae
                                        , "relative_error_tolerance=" ++ show re
                                        , "number_of_steps="          ++ show ss ]
instance TaylorCompat InitialDeclaration where
  taylorSource = show . idExpr

instance (Eq d, Num d, Show d) => TaylorCompat (SSystem d) where
  taylorSource (SSystem inits derivs) = unlines [
    "initial_values = " ++ intercalate ", " (map taylorSource inits) ++ ";\n"
                                , (clines . map taylorSource) derivs ]

instance (Eq d, Num d, Show d) => TaylorCompat (PTerm d) where
  taylorSource (C c) = show c
  taylorSource (v :^: 1) = v
  taylorSource (v :^: p) = v ++ " ^ " ++ show p

instance (Eq d, Num d, Show d) => TaylorCompat (PowerLawForm d) where
  taylorSource = \case
    PLawF v xs [] -> "diff(" ++ v ++ ", t) = " ++ showProd xs
    PLawF v xs ys -> "diff(" ++ v ++ ", t) = " ++ showProd xs ++ " - " ++ showProd ys
    where showProd = intercalate " * " . map taylorSource
