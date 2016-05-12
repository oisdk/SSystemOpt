{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module SSystem where

import           Control.Lens
import           Data.Serialize
import           GHC.Generics
import           Data.Square
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

-- getParams :: SSystem NumLearn -> [[Double]]
-- getParams = toList . getNumLearn <=< toList

-- withParams :: SSystem NumLearn -> [Double] -> Either String (SSystem Double)
-- withParams = evalStateT . traverse f where
--   f = either pure ((const . StateT) (maybe err Right . uncons)) . getNumLearn
--   err = Left "Parameters and ssystem unmatched"
