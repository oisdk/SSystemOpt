{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module SSystem where

import           Control.Lens
import           Data.Foldable
import           Data.Sequence   (Seq)
import qualified Data.Sequence   as Seq
import           GHC.Generics    (Generic)
import           Test.QuickCheck

data SRow a = SRow
  { _posFac :: a
  , _negFac :: a
  , _posExp :: Seq a
  , _negExp :: Seq a
  } deriving (Functor, Foldable, Traversable, Generic, Show, Eq, Ord)

makeLenses ''SRow

data SSystem a = SSystem
  { _odes     :: Seq (SRow a)
  , _initials :: Seq a
  } deriving (Functor, Foldable, Traversable, Generic, Show, Eq, Ord)

makeLenses ''SSystem

toEqns :: (Floating a, Eq a) => (Int -> a) -> SSystem a -> Seq a
toEqns toNum (SSystem s _) = imap f s where
  f i (SRow pf nf pe ne) = toEqn pf pe `subS` toEqn nf ne where
    toEqn facs exps = foldl' mulS facs (imap (\j x -> if i == j then 1 else powS (toNum j) x)  exps)
    powS _ 0 = 1
    powS 1 _ = 1
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

instance Arbitrary a => Arbitrary (SSystem a) where
  arbitrary = sized sgen where
    sgen n = SSystem <$> seqOf rowGen <*> seqOf arbitrary where
      rowGen =
        SRow <$> arbitrary
             <*> arbitrary
             <*> seqOf arbitrary
             <*> seqOf arbitrary
      seqOf :: forall f a. Applicative f => f a -> f (Seq a)
      seqOf = Seq.replicateA n

size :: SSystem a -> Int
size (SSystem rws _) = Seq.length rws
