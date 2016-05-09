{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Expr
  ( Expr(..)
  , Func
  , eval
  , safeEval
  , fromDouble
  , anaM
  , prettyPrint
  , cataM
  ) where

import           Control.Monad         ((<=<))
import           Data.Functor          (void)
import           Data.Functor.Foldable hiding (Foldable, fold, unfold)
import qualified Data.Functor.Foldable as Functor
import           Data.Serialize
import           GHC.Generics          (Generic)
import           Prelude
import           Test.QuickCheck
import Control.Arrow
import Control.Comonad.Cofree

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

zzipo :: (Functor.Foldable g, Functor.Foldable h)
      => (Base g x -> x)                            -- ^ First helper function
      -> (Base h y -> y)                            -- ^ second
      -> (Base g (x, h -> c) -> Base h (y, h) -> c) -- ^ An algebra for two Foldables
      -> g                                          -- ^ first fixed point
      -> h                                          -- ^ second
      -> c                                          -- ^ result
zzipo pa pb alg = zygo pa zalg where
  zalg x = alg x . fmap (cata pb &&& id) . project

pzipo :: (Functor.Foldable g, Functor.Foldable h)
      => (Base g (g, h -> c) -> Base h h -> c) -- ^ An algebra for two Foldables
      -> g                                     -- ^ first fixed point
      -> h                                     -- ^ second
      -> c                                     -- ^ result
pzipo alg = para zalg where zalg x = alg x . project

hzipo :: (Functor.Foldable g, Functor.Foldable h)
      => (Base g (Cofree (Base g) (h -> c)) -> Base h (Cofree (Base h) h) -> c)
      -> g -> h -> c
hzipo alg = histo zalg where
  zalg x = alg x . (para . fmap . uncurry) (:<)

-- approxEq :: Expr -> Expr -> Bool
-- approxEq = hzipo alg where
--   alg (CstF a) (CstF b) = True
--   alg (SumF (SumF a b :< c) r) (SumF)



-- An unfixed expression type
data ExprF r = CstF Double
             | FncF Func r
             | NegF r
             | PowF r r
             | DivF r r
             | PrdF r r
             | SumF r r
             deriving (Functor, Foldable, Traversable,
                       Eq, Ord, Show, Generic)

data Expr =
    Cst Double
  | Func :$: Expr
  | Neg Expr
  | Expr :^: Expr
  | Expr :/: Expr
  | Expr :*: Expr
  | Expr :+: Expr
  deriving (Show, Eq, Ord, Generic)

type instance Base Expr = ExprF

instance Functor.Foldable Expr where
  project = \case
    Cst k   -> CstF k
    f :$: x -> FncF f x
    Neg x   -> NegF x
    x :^: y -> PowF x y
    x :/: y -> DivF x y
    x :*: y -> PrdF x y
    x :+: y -> SumF x y

instance Unfoldable Expr where
  embed = \case
    CstF k   -> Cst k
    FncF f x -> f :$: x
    NegF x   -> Neg x
    PowF x y -> x :^: y
    DivF x y -> x :/: y
    PrdF x y -> x :*: y
    SumF x y -> x :+: y

arbAlg :: Int -> Gen (ExprF Int)
arbAlg size
  | size <= 1 = CstF <$> arbitrary
  | otherwise = oneof
    [ CstF <$> arbitrary
    , flip FncF r <$> arbitrary
    , pure $ NegF r
    , pure $ PowF r r
    , pure $ DivF r r
    , pure $ PrdF r r
    , pure $ SumF r r
    ] where r = size `div` 4

instance Arbitrary Expr where arbitrary = sized (anaM arbAlg)

instance Serialize Expr where
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


-- The supported functions of PLAS
data Func =
    Sin | Cos | Exp | Log | Tan | Atn | Asn
  | Acs | Snh | Csh | Tnh | Ach | Ash | Ath
          deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum

instance Serialize Func
-- Applies a function to a value

instance Show Func where
  show = \case
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

eval :: Expr -> Double
eval = cata evalAlg

evalAlg :: ExprF Double -> Double
evalAlg = \case
  CstF d   -> d
  NegF a   -> negate a
  SumF a b -> a + b
  DivF a b -> a / b
  PrdF a b -> a * b
  PowF a b -> a ** b
  FncF f x -> appF f x

safeEvalAlg :: ExprF Double -> Either String Double
safeEvalAlg = \case
  DivF _ 0 -> Left "Tried to divide by zero"
  e -> Right (evalAlg e)

safeEval :: Expr -> Either String Double
safeEval = cataM safeEvalAlg

prettyPrint :: Expr -> ShowS
prettyPrint = zygo void (pprAlg ((<) . void))

pprAlg :: (ExprF (t, ShowS) -> t -> Bool) -> ExprF (t, ShowS) -> ShowS
pprAlg cmp e = case e of
  CstF i   -> shows i
  NegF a   -> showString "- " . par a
  SumF a b -> par a . showString " + " . par b
  DivF a b -> par a . showString " / " . par b
  PrdF a b -> par a . showString " * " . par b
  PowF a b -> par a . showString " ^ " . par b
  FncF f (_,x) -> shows f . showChar '(' . x . showChar ')'
  where par (c,p) = showParen (cmp e c) p

instance Num Expr where
  fromInteger = Cst . fromInteger
  x + y = x :+: y
  x * y = x :*: y
  abs e = if eval e < 0 then negate e else e
  signum = Cst . signum . eval
  negate = Neg

instance Fractional Expr where
  fromRational = Cst . fromRational
  (/) = (:/:)

instance Floating Expr where
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

appF :: Func -> Double -> Double
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
