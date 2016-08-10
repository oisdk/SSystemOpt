{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module SBML where

import           Control.Lens
import           Data.Foldable
import           Data.Functor
import qualified Data.Sequence as Seq
import qualified Data.Set      as Set
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
  reactions <- rctns
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
          [units, comps, species, parameters, reactions] ] where

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
          , ("initialAmount", fromString ivar)] []

    -- | The node for declaring the reactions.
    rctns = elmt
      "listOfReactions" [] .
      zipWith reaction specNames <$> eqns where
        reaction name eqn = elmt
          "reaction"
          [("id", fromString $ "diff_" ++ name)]
          (removeEmpty (reactantList eqn)) where
            -- | The @removeEmpty@ function is needed because the SBML
            -- standard doesn't allow for empty nodes. The only node
            -- which could be empty is the reactant list.
            removeEmpty [] = [products, kineticLaw]
            removeEmpty xs =
              [elmt "listOfReactants" [] xs, products, kineticLaw]
            products = elmt
              "listOfProducts" []
              [elmt "speciesReference" [("species", fromString name)] []]
            -- | The actual reaction itself. Uses the @Expr@ type's own
            -- function for converting to MathML
            kineticLaw = elmt
              "kineticLaw" []
              [elmt
                 "math"
                 [("xmlns", "http://www.w3.org/1998/Math/MathML")]
                 [mlRep eqn]]
        -- | The reactant list is a list of all the species involved.
        -- it pulls any variables out of a given equation, and then
        -- filters out any variables that are parameters, rather than
        -- species.
        reactantList e =
          [ specNode vrn
          | vrs <- getVars e
          , let vrn = show vrs
          , Set.member vrn specSet ]
        specNode e =
          elmt "speciesReference" [("species", fromString e)] []
        specSet = foldr Set.insert mempty specNames
        -- | Replaces any @NumLearn@s with variable names, popped
        -- using the Source monad.
        noHoles = traverse (either pure (const (fmap varStr pop))) s
        eqns = toEqns (varStr . Seq.index specNames) <$> noHoles

    prams = do
      pnames <- use given
      pure $ elmt "listOfParameters" []
        [elmt "parameter" [("id", fromString n), ("constant", "true")] []
        | n <- pnames]
