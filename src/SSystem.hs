{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module SSystem where

import           Control.Arrow
import           Control.Lens
import           Control.Monad.Reader
import           Data.Serialize
import           Data.Square
import           GHC.Generics
import           Numeric.Expr
import           Test.QuickCheck
import           Utils

data STerm a =
  STerm { _pos     :: a
        , _neg     :: a
        , _initial :: a
        , _name    :: String
        } deriving (Functor, Foldable, Traversable, Generic, Show, Eq
                   , Ord)

makeLenses ''STerm

instance Arbitrary a => Arbitrary (STerm a) where
  arbitrary = STerm <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Serialize a => Serialize (STerm a)

data SSystem a =
  SSystem { _exponents :: Square (a,a)
          , _terms     :: [STerm a]
          } deriving (Functor, Foldable, Generic, Show, Eq, Ord
                     , Traversable)

makeLenses ''SSystem

instance Arbitrary a => Arbitrary (SSystem a) where
  arbitrary = sized $ \n ->
    SSystem <$> create n arbitrary
            <*> replicateA n arbitrary

instance Serialize a => Serialize (SSystem a)

type NumLearn = Either Double [Double]

data SSystemState a = SSystemState
  { _variables :: [String]
  , _square    :: Square (a,a) }

makeLenses ''SSystemState

toEqns :: (Eq a, Floating a, CanVar a) => SSystem a -> [a]
toEqns (SSystem q t) = runReader (itraverse f t) (SSystemState (t^..traversed.name) q) where
  size = q^.squareSize - 1
  inds = [0..size]
  f l (STerm p n _ _) = do
    bind <- traverse (preview . (square .) . ix . flip (,) l) inds
    exps <- views variables (zip (bind^..traversed._Just))
    let lhs = foldr (combine _1) p exps
    let rhs = foldr (combine _2) n exps
    pure (subS lhs rhs)
  combine side =
    mulS . uncurry powS . (view side *** review _Var)
  powS 1 _ = 1
  powS _ 0 = 1
  powS x 1 = x
  powS x y = x ** y
  mulS 0 _ = 0
  mulS _ 0 = 0
  mulS 1 x = x
  mulS x 1 = x
  mulS x y = x * y
  subS x 0 = x
  subS 0 x = -x
  subS x y = x - y

toEqVars :: (Eq a, Floating a) => SSystem (Either a b) -> Source String [VarExpr a]
toEqVars = fmap toEqns . traverse (either (pure . Lit) (const $ fmap (review _Var) pop))
