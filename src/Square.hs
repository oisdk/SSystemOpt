{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Square
  ( Square
  , squareSize
  , _squareSize
  , create
  , fromList
  , unsafeIndex
  ) where

import           Control.Lens
import           Data.Foldable   (traverse_)
import           Data.Maybe      (mapMaybe)
import           Data.Monoid     ((<>))
import           Data.Serialize
import           Prelude.Extras
import           Test.QuickCheck
import           Utils

-- | This type represents a square matrix. In the form:
--
-- > Square_ None Identity a
--
-- It is unable to be a non-square. It is adapted from
-- <http://www.usma.edu/eecs/SiteAssets/SitePages/Faculty%20Publication%20Documents/Okasaki/icfp99square.pdf Okasaki's>
-- square matrices:
--
-- > data Square_ v w a =
-- >    Zero (v (v a))
-- >  | Even (Square_      v     (Pair w w) a)
-- >  | Odd  (Square_ (Pair v w) (Pair w w) a)
--
-- The extra field in the @Zero@ branch is a function which, when
-- given a valid index, produces a 'Lens' into the element at that
-- position. This could be accomplished without the field, but this
-- way allows sharing of work across repeated access to different
-- indices.
data Square a =
  Square { _squareSize :: Int
         , _square     :: Square_ None Identity a
         } deriving (Functor, Foldable, Traversable)

squareSize :: Getter (Square a) Int
squareSize = to _squareSize

data Square_ v w a =
    Zero (forall b. Int -> Lens' (v b) b) (v (v a))
  | Even (Square_      v     (Pair w w) a)
  | Odd  (Square_ (Pair v w) (Pair w w) a)
  deriving (Functor)

-- In order to store the information in a square, three new types are
-- needed:
--
-- * a type for storing no elements: 'None'
-- * a type for storing one element: 'Identity' (from
-- "Data.Functor.Identity")
-- * a type for storing two sub-trees of elements: 'Pair'

newtype None a = None ()
  deriving (Functor, Foldable, Traversable, Eq, Ord)

data Pair v w a = Pair (v a) (w a)
  deriving (Functor, Foldable, Traversable, Eq, Ord)

-- | The 'create_' function is really two functions combined into one:
-- a normal creation function, with the signature:
--
-- >    (forall b. b -> v b)
-- > -> (forall b. b -> w b)
-- > -> a
-- > -> Int
-- > -> Square_ v w a
--
-- And an indexing function for a lens, with the signature:
--
-- >    (forall b. Int -> Lens' (v b) b)
-- > -> (forall b. Int -> Lens' (w b) b)
-- > -> Int -> Int
--
-- building the indexing function allows for sharing between access
-- of different indices.

mkP :: Applicative f => (f a -> f (v a)) -> (f a -> f (w a)) -> f a -> f (Pair v w a)
mkP mkv mkw x = Pair <$> mkv x <*> mkw x

create ::Applicative f => Int -> f a -> f (Square a)
create n =
  fmap (Square n) . create_ leE leI 0 1 ((const.pure.None) ()) (fmap Identity) n

create_ :: Applicative f
        => (forall b. Int -> Lens' (v b) b)
        -> (forall b. Int -> Lens' (w b) b)
        -> Int -> Int
        -> (forall b. f b -> f (v b))
        -> (forall b. f b -> f (w b))
        -> Int
        -> f a
        -> f (Square_ v w a)
create_ lev _ _ _ mkv _ 0 = fmap (Zero lev) . mkv . mkv
create_ lev lew vsz wsz mkv mkw n
  | even n = fmap Even .
    create_
      lev (leP lew lew wsz) vsz (wsz+wsz)
      mkv (mkP mkw mkw) (n `div` 2)
  | otherwise = fmap Odd .
    create_
      (leP lev lew vsz) (leP lew lew wsz) (vsz+wsz) (wsz+wsz)
      (mkP mkv mkw) (mkP mkw mkw) (n `div` 2)
-- The indexing 'Lens' for 'None'. If this is called, it means an
-- invalid index has been given. This is caught earlier with the
-- 'Square' indexing functions.
leE :: Int -> Lens' (None a) a
leE _ = undefined

-- The indexing 'Lens' for 'Identity'. This should only recieve @0@ as
-- the index, however this is not checked, as it is check earlier.
leI :: Int -> Lens' (Identity a) a
leI _ f (Identity x) = Identity <$> f x

-- The indexing 'Lens' for a pair.
leP :: (Int -> Lens' (v a) a)
    -> (Int -> Lens' (w a) a)
    -> Int -> Int -> Lens' (Pair v w a) a
leP lev lew nv i f (Pair v w)
  | i < nv    = flip Pair w <$> lev i f v
  | otherwise = Pair v <$> lew (i-nv) f w

-- Indexing
type FlippedLens' s a = forall f. Functor f => s -> (a -> f a) -> f s

ix_ :: (Int, Int) -> Lens' (Square_ v w a) a
ix_ (i,j) = flip ix' where
  ix' :: FlippedLens' (Square_ v w a) a
  ix' (Zero lev vv) = \f -> Zero lev <$> (lev i . lev j) f vv
  ix' (Even      m) = fmap Even . ix' m
  ix' (Odd       m) = fmap Odd  . ix' m

instance Ixed (Square a) where
  ix (i,j) f (Square n m)
    | i >= n    = pure (Square n m)
    | j >= n    = pure (Square n m)
    | otherwise = Square n <$> ix_ (i,j) f m

type instance Index (Square a) = (Int, Int)
type instance IxValue (Square a) = a

unsafeIndex :: Square a -> (Int, Int) -> a
unsafeIndex (Square _ s) i = s ^. ix_ i

-- Instances

-- Foldable and Traversable
instance (Foldable v, Foldable w) => Foldable (Square_ v w) where
  foldMap f (Zero _ v) = (foldMap.foldMap) f v
  foldMap f (Even   v) = foldMap f v
  foldMap f (Odd    v) = foldMap f v
  foldr f i (Zero _ v) = foldr (flip $ foldr f) i v
  foldr f i (Even   v) = foldr f i v
  foldr f i (Odd    v) = foldr f i v

instance (Traversable v, Traversable w)
  => Traversable (Square_ v w) where
    traverse f (Zero l v) = Zero l <$> (traverse.traverse) f v
    traverse f (Even   v) = Even   <$> traverse            f v
    traverse f (Odd    v) = Odd    <$> traverse            f v

-- Eq and Ord

-- Eq1
instance Eq1 None where _ ==# _ = True

instance (Eq1 v, Eq1 w) => Eq1 (Pair v w) where
  Pair a b ==# Pair c d = a ==# c && b ==# d

-- Ord1
instance Ord1 None where compare1 _ _ = EQ

instance (Ord1 v, Ord1 w) => Ord1 (Pair v w) where
  compare1 (Pair a b) (Pair c d) = compare1 a c <> compare1 b d

-- EqR1
class Eq1 f => EqR1 f where
  eqR1 :: (Eq a, EqR1 g) => f (g a) -> f (g a) -> Bool

instance EqR1 None where eqR1 _ _ = True

instance EqR1 Identity where
  eqR1 (Identity a) (Identity b) = a ==# b

instance (EqR1 v, EqR1 w) => EqR1 (Pair v w) where
  eqR1 (Pair a b) (Pair c d) = eqR1 a c && eqR1 b d

-- OrdR1
class (Ord1 f, EqR1 f) => OrdR1 f where
  cmpR1 :: (Ord a, OrdR1 g) => f (g a) -> f (g a) -> Ordering

instance OrdR1 None where cmpR1 _ _ = EQ

instance OrdR1 Identity where
  cmpR1 (Identity a) (Identity b) = compare1 a b

instance (OrdR1 v, OrdR1 w) => OrdR1 (Pair v w) where
  cmpR1 (Pair a b) (Pair c d) = cmpR1 a c <> cmpR1 b d

-- Eq instances for 'Square'
instance (EqR1 v, EqR1 w) => Eq1 (Square_ v w) where
  Zero _ x ==# Zero _ y = eqR1 x y
  Odd    x ==# Odd    y = x ==# y
  Even   x ==# Even   y = x ==# y
  _        ==# _        = False

instance Eq a => Eq (Square a) where
  Square n v == Square m w = n == m && v ==# w

instance Eq1 Square where
  Square n v ==# Square m w = n == m && v ==# w

-- Ord instances for 'Square'
instance (OrdR1 v, OrdR1 w) => Ord1 (Square_ v w) where
  compare1 (Zero _ x) (Zero _ y) = cmpR1 x y
  compare1 (Odd    x) (Odd    y) = compare1 x y
  compare1 (Even   x) (Even   y) = compare1 x y
  -- Differently sized squares are compared on their sizes
  compare1  _          _         = undefined

instance Ord a => Ord (Square a) where
  compare (Square n v) (Square m w) = compare n m <> compare1 v w

instance Ord1 Square where
  compare1 (Square n v) (Square m w) = compare n m <> compare1 v w

 -- Show
instance Show a => Show (Square a) where
  showsPrec _ (Square n m) =
    flip (foldr (\e a -> e ('\n':a)))
      [ showList [ m ^. ix_ (i,j) | j <- idxs] | i <- idxs]
        where idxs = [0..(n-1)]

-- Arbitrary
instance Arbitrary a => Arbitrary (Square a) where
  arbitrary = sized $ \n -> do
    d <- wobble n <$> choose (0,n `div` 4) <*> arbitrary
    create d arbitrary
    where
      wobble n m LT = n-m
      wobble n _ EQ = n
      wobble n m GT = n+m
  shrink s@(Square n _) =
    mapMaybe (`fromList` foldr (:) [] s) (shrink n)

-- Serialize
instance Serialize a => Serialize (Square a) where
  put s@(Square n _) = put n *> traverse_ put s
  get = create <$> get >>= ($get)

-- fromList

fromList :: Foldable f => Int -> f a -> Maybe (Square a)
fromList n = evalSource (create n pop)
