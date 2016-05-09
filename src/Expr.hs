{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TypeFamilies      #-}

module Expr
  ( Expr(..)
  , ExprF(..)
  , Func
  , app
  , sin
  , cos
  , exp
  , log
  , tan
  , atan
  , sinh
  , cosh
  , tanh
  , (^)
  , (/)
  , eval
  , safeEval
  , fromDouble
  , anaM
  , cataM
  ) where

import           Control.Monad         ((<=<))
import           Data.Functor          (void)
import           Data.Functor.Foldable hiding (Foldable, fold, unfold)
import qualified Data.Functor.Foldable as F
import           Data.Serialize
import           GHC.Generics
import           Prelude               hiding (atan, cos, cosh, exp, log, sin,
                                        sinh, tan, tanh, (/), (^))
import qualified Prelude               as P
import           Test.QuickCheck
-- | A monadic catamorphism.
cataM
  :: (F.Foldable t, Traversable (Base t), Monad m)
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

-- An unfixed expression type
data ExprF r = CstF Double
             | FncF Func r
             | NegF r
             | PowF r r
             | DivF r r
             | PrdF r r
             | SumF r r
             deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Generic)

-- An Expression type
newtype Expr = Expr { getExpr :: Fix ExprF } deriving (Eq, Generic)

type instance Base Expr = ExprF
instance F.Foldable Expr where project = fmap Expr . project . getExpr
instance Unfoldable Expr where embed = Expr . embed . fmap getExpr

arbAlg :: Int -> Gen (ExprF Int)
arbAlg size
  | size <= 1 = (CstF . abs) <$> arbitrary
  | otherwise = oneof
    [ (CstF . abs) <$> arbitrary
    , flip FncF r <$> arbitrary
    , pure $ NegF r
    , pure $ PowF r r
    , pure $ DivF r r
    , pure $ PrdF r  r
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
  get = anaM (const (getWord8 >>= alg)) () where
    alg = \case
      0 -> CstF <$> get
      1 -> flip FncF () <$> get
      2 -> pure $ NegF ()
      3 -> pure $ PowF () ()
      4 -> pure $ DivF () ()
      5 -> pure $ PrdF () ()
      6 -> pure $ SumF () ()
      _ -> error "Corrupted binary"


-- The supported functions of PLAS
data Func = Sin
          | Cos
          | Exp
          | Log
          | Tan
          | Atn
          | Snh
          | Csh
          | Tnh
          deriving (Eq, Ord, Enum, Bounded, Generic)

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum

instance Serialize Func
-- Applies a function to a value
appF :: Func -> Double -> Double
appF = \case
  Exp -> P.exp
  Sin -> P.sin
  Cos -> P.cos
  Tan -> P.tan
  Log -> P.log
  Atn -> P.atan
  Snh -> P.sinh
  Csh -> P.cosh
  Tnh -> P.tanh

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

infixr 8 ^
(^)  :: Expr -> Expr -> Expr
Expr a ^ Expr b = (Expr . Fix) (PowF a b)
infixl 7 /
(/)  :: Expr -> Expr -> Expr
Expr a / Expr b = (Expr . Fix) (DivF a b)
app  :: Func -> Expr -> Expr
app f (Expr x) = (Expr . Fix) (FncF f x)
fromDouble :: Double -> Expr
fromDouble = Expr . Fix . CstF

-- Shadows of prelude math functions which work on expressions

sin, cos, exp, log, tan, atan, sinh, cosh, tanh :: Expr -> Expr
sin  = app Sin
cos  = app Cos
exp  = app Exp
log  = app Log
tan  = app Tan
atan = app Atn
sinh = app Snh
cosh = app Csh
tanh = app Tnh

eval :: Expr -> Double
eval = cata evalAlg

evalAlg :: ExprF Double -> Double
evalAlg = \case
  CstF d   -> d
  NegF a   -> negate a
  SumF a b -> a + b
  DivF a b -> a P./ b
  PrdF a b -> a * b
  PowF a b -> a ** b
  FncF f x -> appF f x

safeEvalAlg :: ExprF Double -> Either String Double
safeEvalAlg = \case
  DivF _ 0 -> Left "Tried to divide by zero"
  e -> Right (evalAlg e)

safeEval :: Expr -> Either String Double
safeEval = cataM safeEvalAlg

instance Show Expr where showsPrec _ = zygo void (pprAlg ((<) . void))

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
  fromInteger     = ana CstF . fromInteger
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs e           = if eval e < 0 then negate e else e
  signum          = ana CstF . signum . eval
  negate          = Expr . Fix . NegF . getExpr

