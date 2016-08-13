{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module SBML where

import           Control.Lens
import           Data.Functor
import qualified Data.Sequence as Seq
import           Data.String
import           Numeric.Expr
import           Prelude       hiding (readFile, writeFile, zipWith)
import           SSystem
import           Text.Taggy
import           Utils

-- | Module for the creation of SBML from an SSystem. Here, "species"
-- refers to the variables in an SSystem: the top-level variables which
-- each have a corresponding ODE. The "parameters", on the other hand,
-- are the constants which would vary from simulation to simulation.

toSBML :: SSystem (Either (VarExpr Double) a) -> Node
toSBML s = evalUniques $ do
  -- Removes the names of the species from the source. These names
  -- cannot be "popped" off as they are needed, as they are needed
  -- in more than one place, so that would cause the same species
  -- to have different names.
  source %= dropStream numSpecs
  -- Each of the following is wrapped in the @Source@ monad, as they
  -- each require unique names in their construction.
  assignments <- rules
  species <- specs
  parameters <- prams
  -- Final SBML
  pure $
    elmt "sbml"
      [ ("level"  , "2")
      , ("version", "3")
      , ("xmlns"  , "http://www.sbml.org/sbml/level2/version3") ]
      [ elmt "model"
          [ ("name", "ssystem") ]
          [units, comps, species, parameters, assignments] ] where

    varStr :: String -> VarExpr Double
    varStr = Var . fromString
    numSpecs = size s
    specNames = Seq.fromList (takeStream numSpecs uniqNames)
    elmt x y z = NodeElement (Element x y z)

    -- | Filler unit, to satisfy the SBML standard
    units = elmt
      "listOfUnitDefinitions" []
      [elmt "unitDefinition" [("id", "sunit")] []]

    -- | Filler compartment, to satisfy the SBML standard
    comps = elmt
      "listOfCompartments" []
      [elmt "compartment" [("id", "main")] []]

    -- | The node for declaring the species in the system. It declares
    -- the name of the species, and its initial value. It needs to
    -- be wrapped in the @Source@ monad in case any of the initial
    -- values contain parameters.
    specs = elmt "listOfSpecies" [] <$>
        zipWithA species (s^.initials) specNames where
      species ival name = do
        ivar <- either (pure.show) (const pop) ival
        pure $ elmt "species"
          [ ("compartment", "main")
          , ("id", fromString name)
          , ("name", fromString name)
          , ("initialConcentration", fromString ivar)] []

    -- | The node for declaring the reactions.
    rules = elmt
      "listOfRules" [] .
      zipWith rule specNames <$> eqns where
        rule name eqn = elmt
          "assignmentRule"
          [("variable", fromString name)]
          [elmt
              "math"
              [("xmlns", "http://www.w3.org/1998/Math/MathML")]
              [mlRep (eqn * varStr "time")]]
        -- | Replaces any @NumLearn@s with variable names, popped
        -- using the Source monad.
        noHoles = traverse (either pure (const (fmap varStr pop))) s
        eqns = toEqns (varStr . Seq.index specNames) <$> noHoles

    prams = do
      pnames <- use given
      pure $ elmt "listOfParameters" []
        [elmt "parameter" [("id", fromString n), ("constant", "true")] []
        | n <- pnames]
