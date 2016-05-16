{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module SBML where

import Text.Taggy
import SSystem
import Data.String
import Numeric.Expr

toSBML :: (MathML a, Eq a, Floating a, Show a) => SSystem a -> Node
toSBML s =
  NodeElement (
    Element "sbml"
            [("level", "2"),
             ("version", "3"),
             ("xmlns", "http://www.sbml.org/sbml/level2/version3")]
            [NodeElement (Element "model"
                                  [("name", "ssystem")]
                                  [units, comps, specs, rxns])]) where
  units = NodeElement (Element "listOfUnitDefinitions" []
                       [NodeElement (Element "unitDefinition" [("id", "sunit")] [])])
  comps = NodeElement (Element "listOfCompartments" []
                       [NodeElement (Element "compartment" [("id", "main")] [])])
  specs = NodeElement (Element "listOfSpecies" []
                       (map (NodeElement . sp) (_terms s)))
  sp (STerm _ _ i n) = Element "species" [ ("compartment", "main")
                                         , ("id", fromString n)
                                         , ("initialAmount", (fromString.show) i)
                                         , ("name", fromString n) ] []
  rxns = NodeElement (Element "listOfReactions" []
                      (zipWith rx (_terms s) (toEqns s)))
  rx (STerm _ _ _ n) e = NodeElement $ Element "reaction" [("id", fromString $ "diff " ++ n)]
    [ NodeElement $ Element "listOfReactants" [] (map v (gv e))
    , NodeElement $ Element "listOfProducts" [] [NodeElement $ Element "speciesReference"
                                                 [("species", fromString n)] []]
    , NodeElement $ Element "kineticLaw" [] [NodeElement $ Element "math"
                                             [("xmlns", "http://www.w3.org/1998/Math/MathML")]
                                              [mlRep e]]]
  gv = either pure (foldMap gv) . _getVarExpr
  v e = NodeElement (Element "speciesReference" [("species", fromString e)] [])
