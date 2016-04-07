{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module SSystem where

data STerm a = STerm { posFac :: a
                     , negFac :: a
                     , posExp :: [a]
                     , negExp :: [a]
                     , initVa :: a } deriving (Functor, Foldable, Traversable)

newtype SSystem a = SSystem { getSSystem :: [STerm a]
                            } deriving (Functor, Foldable, Traversable)



data Parameter a = Parameter { simVal :: a
                             , valRange :: [a] }

