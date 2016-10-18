{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}

module Text.Parse.ODEBench
  ( Problem(..)
  , problem
  , Dependency(..)
  , Sample(..)
  , Bounds(..)
  , ParamInfo(..)
  , fromFile
  , parseTest
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.List                    (sortOn, zipWith5)
import           Data.Maybe
import           Data.Monoid
import           Data.Traversable
import           Data.Utils
import           Data.Vector.Storable         (Vector)
import qualified Data.Vector.Storable         as Vector
import           GHC.Exts                     (IsString (..), fromList)
import           Text.Parser.Char             hiding (lower, upper)
import           Text.Parser.Combinators
import           Text.Parser.Token            hiding (double)
import qualified Text.Parser.Token            as Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.PrettyPrint.ANSI.Leijen (plain, putDoc)
import           Text.Trifecta.Parser         hiding (Parser, parseTest)
import qualified Text.Trifecta.Parser         as Trifecta
import           Text.Trifecta.Result

-- | Module for parsing the problems stated
-- at http://www.cse.chalmers.se/~dag/identification/Benchmarks/

-- $setup
-- >>> let p prsr = parseTest (whiteSpace *> prsr <* eof)

newtype Parser a = Parser
  { getParser :: Trifecta.Parser a
  } deriving (Functor, Applicative, Monad, Alternative
             ,Parsing, CharParsing, MonadPlus)

-- | IsString instance for nice syntax
instance (a ~ ()) => IsString (Parser a) where
   fromString = reserved

-- | Tests a parser without ansi-colorization for error messages.
parseTest :: Show a => Parser a -> String -> IO ()
parseTest p s =
  case parseString (getParser p) mempty s of
    Success x -> print x
    Failure (ErrInfo d _) -> putDoc (plain d)


instance TokenParsing Parser where
  someSpace = Parser $
    buildSomeSpaceParser
      someSpace
      (CommentStyle "" "" "//" False)

identStyle :: IdentifierStyle Parser
identStyle = IdentifierStyle
  "identifier"
  letter
  (alphaNum <|> char '_')
  (fromList [])
  Identifier
  ReservedIdentifier

identifier :: Parser String
identifier = ident identStyle

-- | >>> p (reserved "hello") "hgello"
-- (interactive):1:1: error: expected: "hello"
-- hgello<EOF>
-- ^
reserved :: String -> Parser ()
reserved = reserve identStyle

-- | Parses special syntax which allows trailing dots
-- >>> p double "20."
-- 20.0
-- >>> p double "2"
-- 2.0
-- >>> p double "-2"
-- -2.0
-- >>> p double "-2."
-- -2.0
double :: Parser Double
double = try Token.double <|> do
  num <- integer'
  skipOptional (char '.')
  whiteSpace
  pure (fromInteger num)

-- | >>> p (property "lowerBound" *> double) "has lowerBound = 0.20000000E+00"
-- 0.2
property :: String -> Parser ()
property s = do
  "has"
  reserved s
  "="

simpleProp :: String -> String -> Parser Double
simpleProp v p = reserved v *> property p *> double

context :: Parser a -> Parser a
context = ("of" *>)

data Dependency = Dependent
                | Input
                deriving (Show, Eq, Ord)

-- | >>> p dependency "is dependent"
-- Dependent
-- >>> p dependency "is inputVariable"
-- Input
dependency :: Parser Dependency
dependency = "is"
          *> (Dependent <$ "dependent"
         <|>  Input     <$ "inputVariable")

ind :: Parser Integer
ind = char '_' *> decimal

-- | >>> p (indexedOneDim "alpha") "alpha_1"
-- 1
indexedOneDim :: String -> Parser Integer
indexedOneDim s = token (string s *> ind)

-- | >>> p variable "variable_1 has name = x1 is dependent"
-- (1,Dependent)
variable :: Parser (Integer, Dependency)
variable = (,) <$> indexedOneDim "variable"
               <*  property "name"
               <*  identifier
               <*> dependency

-- | >>> p experiment "experiment_1 has name = exp1 has perfectData"
-- 1
experiment :: Parser Integer
experiment = indexedOneDim "experiment"
          <* property "name"
          <* identifier
          <* "has"
          <* "perfectData"

-- | >>> p ((,) <$> indexedOneDim "sample" <*> context (indexedOneDim "experiment")) "sample_2 \nof experiment_1"
-- (2,1)
data Sample = Sample
  { sampleNum     :: !Integer
  , experimentNum :: !Integer
  , time          :: !Double
  , values        :: !(Vector Double)
  , stdDevs       :: !(Vector Double)
  } deriving (Eq, Ord)

instance Show Sample where
  show (Sample s e t vrs std) = concat
    [ "Sample "
    , show s
    , " of experiment "
    , show e
    , " at time "
    , show t
    , "\n"
    , show vrs
    , "\n"
    , show std ]

-- | >>> p (vec 4) "0.13  0.6036232  0.11150  0.75"
-- [0.13,0.6036232,0.1115,0.75]
vec :: Int -> Parser (Vector Double)
vec n = Vector.replicateM n double

-- | >>> :{
-- let s = unlines
--         [ "sample_2 of experiment_1"
--         , " has time =  0.20000000E+00"
--         , " has variable_ =   0.131E+01  0.6036  0.1115  0.75"
--         , " has sdev of variable_ =  0.131 0.6 0.11 0.0" ]
-- in p (sample 4) s
-- :}
-- Sample 2 of experiment 1 at time 0.2
-- [1.31,0.6036,0.1115,0.75]
-- [0.131,0.6,0.11,0.0]
sample :: Int -> Parser Sample
sample n = Sample <$> indexedOneDim "sample"
                  <*> context (indexedOneDim "experiment")
                  <*  property "time"
                  <*> double
                  <*  property "variable_"
                  <*> vec n
                  <*> foldr (*>) (vec n) ["has","sdev","of","variable_","="]

data ParamInfo a = ParamInfo
  { alphas  :: [a]
  , betas   :: [a]
  , posExps :: [[a]]
  , negExps :: [[a]]
  } deriving (Eq, Ord, Functor, Foldable)

instance Show a => Show (ParamInfo a) where
  showsPrec _ p = showTable r 2 (n*2+2) hd cl where
    n = length (alphas p)
    r = (maximum . fmap (length.show)) p
    hd = ["a"] ++ [ 'g' : show i | i <- [1..n] ] ++ ["b"] ++ [ 'h' : show i | i <- [1..n] ]
    cl = zipWith5 (\i a ps b ns -> (show i, [a] ++ ps ++ [b] ++ ns))
      ([1..] :: [Int])
      (alphas  p)
      (posExps p)
      (betas   p)
      (negExps p)

data Bounds = Bounds
  { lower :: !Double
  , upper :: !Double
  } deriving (Eq, Ord)

instance Show Bounds where
  showsPrec _ (Bounds l u) =
    showParen True (shows l . showChar ',' . shows u)

paramInfo :: (String -> Parser a)
          -> (String -> Parser b)
          -> (a -> b -> a)
          -> Int -> Parser (ParamInfo a)
paramInfo defInfo prop repDef n =
  ParamInfo
       <$> (map.repDef <$> defInfo "alpha" <*> traverse (coefInfo "alpha") [1..n])
       <*> (map.repDef <$> defInfo "beta"  <*> traverse (coefInfo "beta" ) [1..n])
       <*> (map.map.repDef <$> defInfo "g" <*> traverse (for [1..n] . expInfo "g") [1..n])
       <*> (map.map.repDef <$> defInfo "h" <*> traverse (for [1..n] . expInfo "h") [1..n])
  where
    coefInfo s i = prop (s <> "_" <> show i)
    expInfo s i j = prop (s <> "_" <> show i <> "_" <> show j)

-- | >>> :{
-- let sampleStr = unlines
--         [ " alpha has defaultLowerBound = 20.      "
--         , " alpha has defaultUpperBound =  0.      "
--         , " beta has defaultLowerBound  =  0.      "
--         , " beta has defaultUpperBound  = 20.      "
--         , " g has defaultLowerBound     = -4.      "
--         , " g has defaultUpperBound     =  4.      "
--         , " g_1_1 has lowerBound        =  0.      "
--         , " g_1_1 has upperBound        =  0.      "
--         , " g_2_2 has lowerBound        =  0.      "
--         , " g_2_2 has upperBound        =  0.      "
--         , " h has defaultLowerBound     = -4.      "
--         , " h has defaultUpperBound     =  4.      "
--         , " h_1_1 has lowerBound        =  1.5     "
--         , " h_2_2 has lowerBound        =  1.2     " ]
-- in p (bounds 2) sampleStr
-- :}
-- ┌──┬──────────┬──────────┬──────────┬──────────┬──────────┬──────────┐
-- │  │    a     │    g1    │    g2    │    b     │    h1    │    h2    │
-- ├──┼──────────┼──────────┼──────────┼──────────┼──────────┼──────────┤
-- │ 1│(20.0,0.0)│ (0.0,0.0)│(-4.0,4.0)│(0.0,20.0)│ (1.5,4.0)│(-4.0,4.0)│
-- │ 2│(20.0,0.0)│(-4.0,4.0)│ (0.0,0.0)│(0.0,20.0)│(-4.0,4.0)│ (1.2,4.0)│
-- └──┴──────────┴──────────┴──────────┴──────────┴──────────┴──────────┘
bounds :: Int -> Parser (ParamInfo Bounds)
bounds = paramInfo defaultBound bound repDef where
  bound s =
    (,) <$> optional (simpleProp s "lowerBound")
        <*> optional (simpleProp s "upperBound")
  defaultBound s =
    Bounds <$> simpleProp s "defaultLowerBound"
           <*> simpleProp s "defaultUpperBound"
  repDef Bounds {..} (l,u) = Bounds (fromMaybe lower l) (fromMaybe upper u)

-- | >>> :{
-- let sampleStr = unlines
--         [ " alpha has defaultInitialValue = 20.      "
--         , " beta has defaultInitialValue  =  0.      "
--         , " g has defaultInitialValue     = -4.      "
--         , " h has defaultInitialValue     = -4.      " ]
-- in p (initials 3) sampleStr
-- :}
-- ┌──┬────┬────┬────┬────┬────┬────┬────┬────┐
-- │  │ a  │ g1 │ g2 │ g3 │ b  │ h1 │ h2 │ h3 │
-- ├──┼────┼────┼────┼────┼────┼────┼────┼────┤
-- │ 1│20.0│-4.0│-4.0│-4.0│ 0.0│-4.0│-4.0│-4.0│
-- │ 2│20.0│-4.0│-4.0│-4.0│ 0.0│-4.0│-4.0│-4.0│
-- │ 3│20.0│-4.0│-4.0│-4.0│ 0.0│-4.0│-4.0│-4.0│
-- └──┴────┴────┴────┴────┴────┴────┴────┴────┘
initials :: Int -> Parser (ParamInfo Double)
initials = paramInfo defInit initial fromMaybe where
  defInit s = simpleProp s "defaultInitialValue"
  initial s = optional (simpleProp s "initialValue")

data Problem = Problem
  { variables   :: [Dependency]
  , space       :: ParamInfo Bounds
  , samples     :: [Sample]
  , initialSoln :: ParamInfo Double }

problem :: Parser Problem
problem = do
  whiteSpace
  vars <- some (variable <?> "vars")
  let numVars = length vars
  let varDeps = (map snd . sortOn fst) vars
  Problem    varDeps
         <$> (bounds numVars <?> "bounds")
         <*  some (experiment <?> "exps")
         <*> some (sample numVars <?> "sample")
         <*> initials numVars

fromFile :: String -> IO Problem
fromFile = parseFromFileEx (getParser problem) >=> \case
  Success x -> pure x
  Failure (ErrInfo d _) -> putDoc d *> fail (show $ plain d)
