{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module SSystem where

import           Control.Lens
import           Data.Serialize
import           Data.Square
import           Data.String
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

toEqns :: (Eq a, Floating a) => SSystem a -> [VarExpr a]
toEqns (SSystem q t) = imap (f q) t where
  f :: (Eq a, Floating a) => Square (a,a) -> Int -> STerm a -> VarExpr a
  f _ _ (STerm 0 0 _ _) = 0
  f s i (STerm n 0 _ _) = foldr (*) (vlit n) (m s i _1)
  f s i (STerm 0 n _ _) = foldr (*) (negate $ vlit n) (m s i _2)
  f s i (STerm p n _ _) = foldr (*) (vlit p) (m s i _1) - foldr (*) (vlit n) (m s i _2)
  l s i g = [vlit $ s ^?! ix (i,j) . g | j <- [0..(_squareSize s - 1)]]
  r e = [ fromString x ** p | (x,p) <- zip (map _name t) e, p /= 0]
  m s i g = r $ l s i g
  vlit = VarExpr . Right . LitF
