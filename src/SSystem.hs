{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}

module SSystem where

import           Control.Monad.State
import           Data.Foldable       (toList)
import           Data.List           (uncons)
import           Data.Serialize
import           GHC.Generics
import           Test.QuickCheck

data STerm a = STerm { posFac :: a
                     , negFac :: a
                     , posExp :: [a]
                     , negExp :: [a]
                     , initVa :: a
                     } deriving (Functor, Foldable, Traversable, Generic, Show, Eq)

newtype SSystem a = SSystem { getSSystem :: [STerm a]
                            } deriving (Functor
                                       , Foldable
                                       , Traversable
                                       , Generic
                                       , Eq
                                       , Show)

newtype NumLearn = NumLearn { getNumLearn :: Either Double [Double]
                            } deriving (Generic, Show, Eq)

instance Arbitrary a => Arbitrary (STerm a) where
  arbitrary = STerm <$> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary
                    <*> arbitrary

instance Arbitrary a => Arbitrary (SSystem a) where
  arbitrary = SSystem <$> arbitrary

instance Arbitrary NumLearn where
  arbitrary = NumLearn <$> arbitrary

instance Serialize a => Serialize (STerm a)
instance Serialize a => Serialize (SSystem a)
instance Serialize NumLearn

getParams :: SSystem NumLearn -> [[Double]]
getParams = toList . getNumLearn <=< toList

withParams :: SSystem NumLearn -> [Double] -> Either String (SSystem Double)
withParams = evalStateT . traverse f where
  f = either pure ((const . StateT) (maybe err Right . uncons)) . getNumLearn
  err = Left "Parameters and ssystem unmatched"
