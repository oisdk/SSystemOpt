{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module SBML where

import           Control.Lens
import           Data.String
import           Numeric.Expr
import           SSystem
import           Text.Taggy
import           Utils
import GHC.Exts

toSBML :: (MathML a, Eq a, Floating a, Show a, Eq b) => SSystem (Either a b) -> Node
toSBML s = fj $ flip evalSource un $
  NodeElement .
    Element "sbml"
            [("level", "2"),
             ("version", "3"),
             ("xmlns", "http://www.sbml.org/sbml/level2/version3")] <$>
            sequenceA
              [NodeElement . Element "model"
                                    [("name", "ssystem")] <$> sequenceA
                                    [units, comps, specs, rxns]] where
  units = pure $ NodeElement (Element "listOfUnitDefinitions" []
                       [NodeElement (Element "unitDefinition" [("id", "sunit")] [])])
  comps = pure $ NodeElement (Element "listOfCompartments" []
                       [NodeElement (Element "compartment" [("id", "main")] [])])
  specs = NodeElement . Element "listOfSpecies" [] <$>
                      traverse (fmap NodeElement . sp) (_terms s)
  sp (STerm _ _ i n) = flip (Element "species") [] . fromList <$> sequenceA
    [ pure ("compartment", "main")
    , pure ("id", fromString n)
    , (,) "initialAmount" . fromString . show <$> grab i
    , pure ("name", fromString n) ]
  rxns = NodeElement . Element "listOfReactions" [] . zipWith rx (_terms s) <$> toEqVars s
  rx (STerm _ _ _ n) e = NodeElement $ Element "reaction" [("id", fromString $ "diff " ++ n)]
    [ NodeElement $ Element "listOfReactants" [] (map v (gv e))
    , NodeElement $ Element "listOfProducts" [] [NodeElement $ Element "speciesReference"
                                                 [("species", fromString n)] []]
    , NodeElement $ Element "kineticLaw" [] [NodeElement $ Element "math"
                                             [("xmlns", "http://www.w3.org/1998/Math/MathML")]
                                              [mlRep e]]]
  gv (Var n) = pure n
  gv (RecExpr e) = foldMap gv e
  v e = NodeElement (Element "speciesReference" [("species", fromString e)] [])
  un = filter (`notElem` s^..terms.each.name) uniqNames
  grab :: Either a b -> Source String (VarExpr a)
  grab = either (pure . RecExpr . LitF) (const (Var <$> pop))
  fj (Just x) = x
  fj _ = undefined

  
