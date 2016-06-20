{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module SBML where

import           Control.Lens
import           Data.Functor
import qualified Data.Set          as Set
import           Data.String
import qualified Data.Text as Text
import           Data.Text.IO      (readFile)
import           Data.Text.Lazy    (pack)
import           Data.Text.Lazy.IO (writeFile)
import qualified Data.Text.Lazy.IO as LText
import           Numeric.Expr
import           Parse
import           Prelude           hiding (readFile, writeFile)
import           SSystem
import           Text.Taggy
import           Utils
import Data.Maybe

toSBML :: (MathML a, Eq a, Floating a, Show a, Eq b) => SSystem (Either a b) -> Node
toSBML s = evalUniques outr where
  eqns = toEqVars s
  elmt x y z = NodeElement (Element x y z)
  vars = Set.fromList $ s^..terms.each.name
  outr = do
    reactions <- rctns
    species <- specs
    parameters <- prams
    pure $
      elmt "sbml"
        [ ("level"  , "2")
        , ("version", "3")
        , ("xmlns"  , "http://www.sbml.org/sbml/level2/version3") ]
        [ elmt "model"
            [ ("name", "ssystem") ]
            [units, comps, species, parameters, reactions] ]
  units = elmt "listOfUnitDefinitions" [] [elmt "unitDefinition" [("id", "sunit")] []]
  comps = elmt "listOfCompartments" [] [elmt "compartment" [("id", "main")] []]
  specs = elmt "listOfSpecies" [] <$> (s^.terms & each sp)
  rctns = elmt "listOfReactions" [] . zipWith rx (s^..terms.each.name) <$> eqns
  rx n e = elmt "reaction" [("id", fromString $ "diff_" ++ n)] . catMaybes $
    [ elmt "listOfReactants" [] <$> nilIfEmpty (map v . filter (`Set.member` vars) . map show . getVars $ e)
    , elmt "listOfProducts" [] <$> nilIfEmpty [elmt "speciesReference" [("species", fromString n)] []]
    , elmt "kineticLaw" [] <$> nilIfEmpty [elmt "math" [("xmlns", "http://www.w3.org/1998/Math/MathML")] [mlRep e]]]
  v e = elmt "speciesReference" [("species", fromString e)] []
  prams = do
    pnames <- use given
    pure $ elmt "listOfParameters" []
      [elmt "parameter"
        [("id", fromString n), ("constant", "true")] []
      | n <- pnames]
  sp (STerm _ _ i n) = do
    ia <- either (pure.show) (const pop') i
    pure $ elmt "species"
      [ ("compartment", "main")
      , ("id", fromString n)
      , ("name", fromString n)
      , ("initialAmount", fromString ia)]
      []
  pop' = do
    x <- use (source.streamHead)
    source %= view streamTail
    if Set.member x vars then pop' else (given %= (|> x)) $> x
  nilIfEmpty :: Foldable f => f a -> Maybe (f a)
  nilIfEmpty xs = foldr (\_ _ -> Just xs) Nothing xs

fromFile :: String -> String -> IO ()
fromFile file out =
  either putStrLn (writeFile out . render . toSBML) .
  parseSystem =<< readFile file

-- | For testing
-- >>> cmpFiles m1 mo
-- True
cmpFiles :: String -> String -> IO Bool
cmpFiles infile outfile = do
  outcont <- LText.readFile outfile
  (outcont ==) . either pack (render . toSBML) . parseSystem <$> readFile infile


m1 :: String
m1 = "ExampleModels/Model1/model.txt"

mo :: String
mo = "ExampleModels/Model1/sbml.xml"

data Experiment = Experiment
  { sigma :: Double
  , networkName :: Text.Text
  , step :: Double
  , results :: [VariableHistory]}

data VariableHistory = VariableHistory
  { variableName :: Text.Text
  , measurements :: [VariableMeasurement] }

type VariableMeasurement = Double

-- | Converts an experiment to the required python file
-- format.
-- >>> putStr . Text.unpack $ toExpFormat exampleNet
-- from SloppyCell.ReactionNetworks import *
-- expt = Experiment('expt1')
-- data = {'net1':{
--   'x': {
--     0.0: (0.0, 0.1),
--     1.0: (1.0, 0.1),
--     2.0: (2.0, 0.1),
--   },
--   'y': {
--     0.0: (0.0, 0.1),
--     1.0: (1.0, 0.1),
--     2.0: (2.0, 0.1),
--   },
-- }}
-- expt.set_data(data)

toExpFormat :: Experiment
            -> Text.Text
toExpFormat (Experiment s n st r) = Text.unlines $
  [ "from SloppyCell.ReactionNetworks import *"
  , "expt = Experiment('expt1')"
  , Text.concat ["data = {'", n, "':{"]] ++
  (f =<< r) ++
  ["}}", "expt.set_data(data)"]
  where
    f (VariableHistory vn ms) =
      Text.concat ["  '", vn, "': {"] :
      [ Text.concat ["    ", repr vt, ": (", repr mv, ", ", repr s, "),"]
      | (mv, vt) <- zip ms [0, 0+st..]] ++
      ["  },"]
    repr = Text.pack . show

exampleNet :: Experiment
exampleNet = Experiment 0.1 "net1" 1
  [ VariableHistory "x" [ 0
                        , 1
                        , 2]
  , VariableHistory "y" [ 0
                        , 1
                        , 2]]
