{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module SSystem
 ( PTerm(..)
 , PowerLawForm(..)
 , SSystem(..)
 , Simulation
 , Parameter(..)
 , Configurable(..)
 , InitialDeclaration(..)
 , withParams
 , justParams
 ) where

import           Data.Either    (rights)
import           Data.Binary
import           Expr
import           GHC.Generics
-- | A PTerm is either a constant, or a variable to the power of some
-- constant. Here, it's a Functor, so that arbitrary values can be
-- swapped in for the exponent.
data PTerm d = C Double | String :^: d
  deriving (Functor, Foldable, Traversable, Show, Eq, Generic)

instance Binary d => Binary (PTerm d)

data InitialDeclaration = ID { idName :: String
                             , idExpr :: Expr
                             } deriving (Eq, Generic)

instance Binary InitialDeclaration

instance Show InitialDeclaration where
  show (ID n e) = n ++ " = " ++ show e

-- | A "power-law-form" equation is one variable, its derivative
-- (in the form of the difference of two products of PTerms), and
-- its initial value. Again, this is a Functor, so that arbitrary
-- exponents can be swapped in.
data PowerLawForm d = PLawF { derivOf :: String
                            , left    :: [PTerm d]
                            , right   :: [PTerm d]
                            } deriving (Eq, Functor, Foldable, Traversable, Show, Generic)

instance Binary d => Binary (PowerLawForm d)

-- | An SSystem is the standard SSystem
data SSystem d = SSystem { sinits :: [InitialDeclaration]
                         , system :: [PowerLawForm d]
                         } deriving (Eq, Functor, Foldable, Traversable, Show, Generic)

instance Binary d => Binary (SSystem d)

data Parameter = Param { max :: Double
                       , min :: Double
                       , sim :: Double
                       , nam :: String
                       } deriving (Eq, Show, Generic)

instance Binary Parameter

data Configurable a = Configurable { getConfig  :: SSystem a
                                   , startTimeC :: Double
                                   , stopTimeC  :: Double
                                   , absTolC    :: Double
                                   , relTolC    :: Double
                                   , numSteps   :: Double
                                   } deriving (Eq, Functor, Show, Generic)

instance Binary a => Binary (Configurable a)

type Simulation = Configurable Double

withParams :: (String -> Double) -> Configurable (Either Double Parameter) -> Simulation
withParams m = fmap (either id (m . nam))

justParams :: Configurable (Either Double Parameter) -> [Parameter]
justParams = rights . foldr (:) [] . getConfig
