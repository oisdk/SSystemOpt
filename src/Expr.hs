{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Expr
  ( Expr
  , Func
  , fnc
  , eval
  , safeEval
  , fromDouble
  , anaM
  , prettyPrint
  , cataM
  , approxEqual
  , showFunc
  , roundShow
  ) where

import           Control.Applicative   (liftA2)
import           Control.Lens
import           Control.Monad         ((<=<))
import           Data.Function
import           Data.Functor          (void)
import           Data.Functor.Foldable hiding (Foldable, fold, unfold)
import qualified Data.Functor.Foldable as Functor
import           Data.Ratio
import           Data.Serialize
import           GHC.Generics          (Generic)
import           Prelude
import           Test.QuickCheck

-- The supported functions of PLAS
data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic, Show)

-- An unfixed expression type
data ExprF a r = CstF a
               | FncF Func r
               | NegF r
               | PowF r r
               | DivF r r
               | PrdF r r
               | SumF r r
               deriving (Functor, Foldable, Traversable,
                         Eq, Ord, Show, Generic)

makePrisms ''ExprF

-- | A monadic catamorphism.
cataM
  :: (Functor.Foldable t, Traversable (Base t), Monad m)
  => (Base t a -> m a) -- ^ a monadic (Base t)-algebra
  -> t                 -- ^ fixed point
  -> m a               -- ^ result
cataM f = c where c = f <=< (traverse c . project)

-- | A monadic anamorphism
anaM
  :: (Unfoldable t, Traversable (Base t), Monad m)
  => (a -> m (Base t a))        -- ^ a monadic (Base t)-coalgebra
  -> a                          -- ^ seed
  -> m t
anaM g = a where a = fmap embed . traverse a <=< g

zipo :: (Functor.Foldable g, Functor.Foldable h)
     => (Base g (h -> c) -> Base h h -> c) -- ^ An algebra for two Foldables
     -> g                                  -- ^ first fixed point
     -> h                                  -- ^ second
     -> c                                  -- ^ result
zipo alg = cata zalg where zalg x = alg x . project

type Expr = ExprN Double

roundShow :: Expr -> ShowS
roundShow = zygo void (either pprAlg (shows . f) . matching _CstF) where
  f :: Double -> Integer
  f = floor

assoc :: Expr -> Expr
assoc = rewrite reassoc where
  reassoc (a :+: (b :+: c)) = Just $ (a :+: b) :+: c
  reassoc (a :*: (b :*: c)) = Just $ (a :*: b) :*: c
  reassoc _ = Nothing

approxEqual :: Expr -> Expr -> Bool
approxEqual = zipo alg `on` assoc where
  alg = zipAlg n (==) (&&) False
  n a b = abs (a-b) < 0.01

zipAlg :: (n -> n -> b)
       -> (Func -> Func -> b)
       -> (b -> b -> b)
       -> b
       -> ExprF n (a -> b)
       -> ExprF n a -> b
zipAlg n l c d = alg where
  alg (CstF a  ) (CstF b  ) = n a b
  alg (FncF f x) (FncF g y) = l f g `c` x y
  alg (NegF x  ) (NegF y  ) = x y
  alg (PowF x w) (PowF y z) = x y `c` w z
  alg (DivF x w) (DivF y z) = x y `c` w z
  alg (PrdF x w) (PrdF y z) = x y `c` w z
  alg (SumF x w) (SumF y z) = x y `c` w z
  alg _ _ = d

data ExprN a =
    Cst a
  | Func :$: ExprN a
  | Neg (ExprN a)
  | ExprN a :^: ExprN a
  | ExprN a :/: ExprN a
  | ExprN a :*: ExprN a
  | ExprN a :+: ExprN a
  deriving (Show, Eq, Ord, Generic)

fnc :: Func -> ExprN a -> ExprN a
fnc = (:$:)

type instance Base (ExprN a) = ExprF a

instance Functor.Foldable (ExprN a) where
  project = \case
    Cst k   -> CstF k
    f :$: x -> FncF f x
    Neg x   -> NegF x
    x :^: y -> PowF x y
    x :/: y -> DivF x y
    x :*: y -> PrdF x y
    x :+: y -> SumF x y

instance Unfoldable (ExprN a) where
  embed = \case
    CstF k   -> Cst k
    FncF f x -> f :$: x
    NegF x   -> Neg x
    PowF x y -> x :^: y
    DivF x y -> x :/: y
    PrdF x y -> x :*: y
    SumF x y -> x :+: y

instance Plated (ExprN a) where
  plate f = fmap embed . traverse f . project

arbAlg :: (Num a, Arbitrary a) => Int -> Gen (ExprF a Int)
arbAlg size
  | size <= 1 = CstF . abs <$> arbitrary
  | otherwise = oneof
    [ CstF . abs <$> arbitrary
    , flip FncF r <$> arbitrary
    , pure $ NegF r
    , pure $ PowF r r
    , pure $ DivF r r
    , pure $ PrdF r r
    , pure $ SumF r r
    ] where
  r = size `div` 2

instance (Num a, Arbitrary a) => Arbitrary (ExprN a) where
  arbitrary = sized (anaM arbAlg)

instance Serialize a => Serialize (ExprN a) where
  put = cata alg where
    alg = \case
      CstF d   -> putWord8 0 *> put d
      FncF f x -> putWord8 1 *> put f *> x
      NegF x   -> putWord8 2 *> x
      PowF x y -> putWord8 3 *> x *> y
      DivF x y -> putWord8 4 *> x *> y
      PrdF x y -> putWord8 5 *> x *> y
      SumF x y -> putWord8 6 *> x *> y
  get = alg =<< getWord8 where
    alg = \case
      0 -> Cst   <$> get
      1 -> (:$:) <$> get <*> get
      2 -> Neg   <$> get
      3 -> (:^:) <$> get <*> get
      4 -> (:/:) <$> get <*> get
      5 -> (:*:) <$> get <*> get
      6 -> (:+:) <$> get <*> get
      _ -> error "corrupted binary"

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum

instance Serialize Func
-- Applies a function to a value

showFunc :: Func -> String
showFunc = \case
  Exp -> "exp"
  Sin -> "sin"
  Cos -> "cos"
  Tan -> "tan"
  Log -> "log"
  Atn -> "atan"
  Snh -> "sinh"
  Csh -> "cosh"
  Tnh -> "tanh"
  Asn -> "asin"
  Acs -> "acos"
  Ach -> "acosh"
  Ash -> "asinh"
  Ath -> "atanh"

fromDouble :: Double -> Expr
fromDouble = Cst

eval :: Floating a => ExprN a -> a
eval = cata evalAlg

evalAlg :: Floating a => ExprF a a -> a
evalAlg = \case
  CstF d   -> d
  NegF a   -> negate a
  SumF a b -> a + b
  DivF a b -> a / b
  PrdF a b -> a * b
  PowF a b -> a ** b
  FncF f x -> appF f x

safeEvalAlg :: ExprF Double Double -> Either String Double
safeEvalAlg = \case
  DivF _ 0 -> Left "Tried to divide by zero"
  e -> Right (evalAlg e)

safeEval :: Expr -> Either String Double
safeEval = cataM safeEvalAlg

prettyPrint :: Expr -> ShowS
prettyPrint = zygo void pprAlg

pprAlg :: (Show a, Ord a) => ExprF a (ExprF a (), ShowS) -> ShowS
pprAlg e = case e of
  CstF i   -> shows i
  NegF a   -> showString "- " . parR a
  SumF a b -> parL a . showString " + " . parL b
  DivF a b -> parR a . showString " / " . parR b
  PrdF a b -> parL a . showString " * " . parL b
  PowF a b -> parR a . showString " ^ " . parR b
  FncF f (_,x) -> shows f . showChar '(' . x . showChar ')'
  where
    parL (c,p) = showParen (void e <  c) p
    parR (c,p) = showParen (void e <= c) p

instance Floating a => Num (ExprN a) where
  fromInteger = Cst . fromInteger
  (+) = (:+:)
  (*) = (:*:)
  abs = Cst . abs . eval
  signum = Cst . signum . eval
  negate = Neg

instance Floating a => Fractional (ExprN a) where
  fromRational = liftA2 (:/:) (fromInteger.numerator) (fromInteger.denominator)
  (/) = (:/:)

instance Floating a => Floating (ExprN a) where
  pi = Cst pi
  exp = (:$:) Exp
  log = (:$:) Log
  sin = (:$:) Sin
  cos = (:$:) Cos
  asin = (:$:) Asn
  acos = (:$:) Acs
  atan = (:$:) Atn
  sinh = (:$:) Snh
  cosh = (:$:) Csh
  asinh = (:$:) Ash
  acosh = (:$:) Ach
  atanh = (:$:) Ath
  (**) = (:^:)

appF :: Floating a => Func -> a -> a
appF = \case
  Exp -> exp
  Sin -> sin
  Cos -> cos
  Tan -> tan
  Log -> log
  Atn -> atan
  Snh -> sinh
  Csh -> cosh
  Tnh -> tanh
  Asn -> asin
  Acs -> acos
  Ach -> acosh
  Ash -> asinh
  Ath -> atanh
