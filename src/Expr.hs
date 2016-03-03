{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeFamilies         #-}

module Expr
  ( Expr(..)
  , ExprF(..)
  , NamedExpr
  , Func
  , ExprType
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
  , eval
  , named
  , unnamed
  , unname
  , fromDouble
  , anaM
  ) where

import           Control.Comonad.Cofree
import           Control.Monad          ((<=<))
import           Data.Binary
import           Data.Functor           (void)
import           Data.Functor.Foldable  hiding (Foldable, fold, unfold)
import qualified Data.Functor.Foldable  as F
import           GHC.Generics
import           Prelude                hiding (atan, cos, cosh, exp, log, sin,
                                         sinh, tan, tanh, (^))
import qualified Prelude                as P
newtype CofreeF f a r = CF { unCofreeF :: (a, f r)
                           } deriving (Functor, Eq, Ord)

type instance Base (Cofree f a) = CofreeF f a

instance Functor f => F.Foldable (Cofree f a) where
  project (a :< c) = CF (a, c)

instance Functor f => Unfoldable (Cofree f a) where
  embed = uncurry (:<) . unCofreeF
  ana alg = unfold (unCofreeF . alg)

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
             | PrdF r r
             | SumF r r
             deriving (Functor, Foldable, Traversable, Eq, Ord, Show, Generic)

-- An Expression type
newtype Expr = Expr { getExpr :: Fix ExprF } deriving (Eq, Generic)

instance Binary Expr where
  put = cata alg . getExpr where
    alg = \case
      CstF d   -> put (0 :: Word8) >> put d
      FncF f x -> put (1 :: Word8) >> put f >> x
      NegF x   -> put (2 :: Word8) >> x
      PowF x y -> put (3 :: Word8) >> x >> y
      PrdF x y -> put (4 :: Word8) >> x >> y
      SumF x y -> put (5 :: Word8) >> x >> y
  get = Expr <$> anaM (const (getWord8 >>= alg)) () where
    alg = \case
      0 -> CstF <$> get
      1 -> flip FncF () <$> get
      2 -> pure $ NegF ()
      3 -> pure $ PowF () ()
      4 -> pure $ PrdF () ()
      5 -> pure $ SumF () ()
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

instance Binary Func
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
class Num e => ExprType e where
  (^)  :: e -> e -> e
  app  :: Func -> e -> e
  eval :: e -> Double
  fromDouble :: Double -> e

-- Shadows of prelude math functions which work on expressions

sin, cos, exp, log, tan, atan, sinh, cosh, tanh :: ExprType e => e -> e
sin  = app Sin
cos  = app Cos
exp  = app Exp
log  = app Log
tan  = app Tan
atan = app Atn
sinh = app Snh
cosh = app Csh
tanh = app Tnh

instance ExprType Expr where
  fromDouble = Expr . Fix . CstF
  Expr a ^ Expr b = (Expr . Fix) (PowF a b)
  app f (Expr x) = (Expr . Fix) (FncF f x)
  eval = cata alg . getExpr where
    alg = \case
      CstF d   -> d
      NegF a   -> negate a
      SumF a b -> a + b
      PrdF a b -> a * b
      PowF a b -> a ** b
      FncF f x -> appF f x

instance Show Expr where showsPrec _ = zygo void (pprAlg ((<) . void)) . getExpr

pprAlg :: (ExprF (t, ShowS) -> t -> Bool) -> ExprF (t, ShowS) -> ShowS
pprAlg cmp e = case e of
  CstF i   -> shows i
  NegF a   -> showChar '-' . par a
  SumF a b -> par a . showString " + " . par b
  PrdF a b -> par a . showString " * " . par b
  PowF a b -> par a . showString " ^ " . par b
  FncF f (_,x) -> shows f . showChar '(' . x . showChar ')'
  where par (c,p) = showParen (cmp e c) p

instance Num Expr where
  fromInteger     = Expr . ana CstF . fromInteger
  Expr a + Expr b = (Expr . Fix) (SumF a b)
  Expr a * Expr b = (Expr . Fix) (PrdF a b)
  abs e           = if eval e < 0 then negate e else e
  signum          = Expr . ana CstF . signum . eval
  negate          = Expr . Fix . NegF . getExpr

newtype NamedExpr = Named { getNamed :: Cofree ExprF (Maybe String)
                          } deriving (Eq)

instance ExprType NamedExpr where
  fromDouble = unnamed . CstF
  Named a ^ Named b = unnamed (PowF a b)
  app f (Named x)   = unnamed (FncF f x)
  eval              = eval . Expr . ana unwrap . getNamed

named :: String -> NamedExpr -> NamedExpr
named s (Named e) = Named (Just s :< unwrap e)

unnamed :: ExprF (Cofree ExprF (Maybe String)) -> NamedExpr
unnamed = Named . (Nothing :<)

unname :: NamedExpr -> Expr
unname = Expr . ana unwrap . getNamed

instance Num NamedExpr where
  fromInteger       = unnamed . CstF . fromInteger
  Named a + Named b = unnamed (SumF a b)
  Named a * Named b = unnamed (PrdF a b)
  abs e             = if eval e < 0 then negate e else e
  signum            = unnamed . CstF . signum . eval
  negate            = unnamed . NegF . getNamed

instance Show NamedExpr where
  showsPrec _ = zygo void alg . getNamed where
    alg   (CF (s, e)) = maybe (pprAlg cmp e) showString s
    cmp e (CF (_, c)) = void e < c

