{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Parse
  ( parseSystem
  , parseTester
  , numLearn
  , ode
  , listOf
  , double'
  , declarations
  , fromParsed
  , parseSystemFromFile
  ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Writer
import           Data.Either
import           Data.Functor
import           Data.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import qualified Data.Sequence               as Seq
import           Numeric.Expr
import           Prelude                     hiding (unlines)
import           SSystem
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Trifecta.Parser
import           Text.Trifecta.Result
import           Utils


-- | Parses a list of values, haskell-style
--
-- >>> parseTester (listOf integer) "[1,2..10]"
-- Right [1,2,3,4,5,6,7,8,9,10]
-- >>> parseTester (listOf double') "[1.0..3.0]"
-- Right [1.0,2.0,3.0]

listOf :: (Enum a, TokenParsing m, Monad m) => m a -> m [a]
listOf parser =
  brackets (unenum =<< commaSep parser) where
    unenum [x] = symbol ".." *> fmap (enumFromTo x) parser <|> pure [x]
    unenum [x,y] =
      symbol ".." *> fmap (enumFromThenTo x y) parser <|> pure [x,y]
    unenum xs = pure xs

-- | Parses the syntax for declaring a parameter.
--
-- >>> parseTester numLearn "[1,2,3]"
-- Right (Right [1.0,2.0,3.0])
numLearn :: (TokenParsing m, Monad m) => m NumLearn
numLearn = eitherA (Lit <$> double') (listOf double')

-- | Parses both doubles and integers, converting them both
-- >>> parseTester double' "23"
-- Right 23.0
-- >>> parseTester double' "1.5"
-- Right 1.5
double' :: TokenParsing m => m Double
double' = either fromInteger id <$> integerOrDouble

identStyle :: TokenParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle
    "variable"
    letter
    alphaNum
    ["ddt"]
    Identifier
    ReservedIdentifier

data ODE =
  ODE { _posFac :: Maybe NumLearn
      , _posExp :: Map String NumLearn
      , _negFac :: Maybe NumLearn
      , _negExp :: Map String NumLearn }

instance Show ODE where
  show (ODE w x y z) = showFac w ++ showMul x ++ " - " ++ showFac y ++ showMul z where
    showFac = maybe "" ((++ " * ") . showNL)
    showMul m = intercalate " * " [ var ++ " ^ " ++ showNL ex | (var,ex) <- Map.assocs m  ]
    showNL = either show show

-- | Parses an ode
-- >>> parseTester ode "2 * x1 ^ 3"
-- Right 2.0 * x1 ^ 3.0 -
-- >>> parseTester ode "-4.5 * x4 ^ 2"
-- Right  - 4.5 * x4 ^ 2.0
ode :: (TokenParsing m, Monad m) => m ODE
ode = odeTup <$> (poss <|> sidel) <*> (negs <|> sidel) where
  facs = (,) <$> optional numLearn
             <*  optional (symbol "*")
             <*> termList
  poss = notFollowedBy (symbol "-") *> facs
  sidel = pure (Nothing, mempty)
  negs = symbol "-" *> facs
  termList = Map.fromList <$> sepBy term (symbol "*")
  term = (,) <$> ident identStyle
             <*> ((symbol "^"
              *> numLearn)
             <|> pure (Left 1.0))
  odeTup (w,x) (y,z) = ODE w x y z

odeDecl :: (Monad m, TokenParsing m) => m (String, ODE)
odeDecl = (,) <$> (reserve identStyle "ddt"
               *> ident identStyle)
              <*> (symbol "=" *> ode)

-- | Parses the declaration of the initial state of a variable
initialValue :: (Monad m, TokenParsing m) => m (String, VarExpr Double)
initialValue = (,) <$> ident identStyle <*> (symbol "=" *> exprParse)

declarations :: (Monad m, TokenParsing m)
           => m [Either (String, ODE) (String, VarExpr Double)]
declarations =
  whiteSpace *>
  semiSep1 (eitherA odeDecl initialValue) <*
  optional semi <*
  eof

matchEqns :: [Either (String, ODE) (String, VarExpr Double)]
          -> Either String (Map String (ODE, VarExpr Double))
matchEqns xs = join $ getMatched <$> cOdes <*> cInits where
  unComma :: [String] -> String
  unComma = intercalate ", "
  catchDupes msg =
    over _Left (\ds -> msg ++ unComma ds) . insertUniques
  cOdes = catchDupes "Duplicate odes for " (lefts xs)
  cInits = catchDupes "Duplicate initial values for " (rights xs)
  getMatched ys zs =
    over _Left (\ds -> "Unmatched equations for " ++ unComma ds) mm
    where mm = mergeMatch (,) ys zs

indexEqns :: Map String (ODE, VarExpr Double)
          -> ([(ODE, VarExpr Double)], Map String Int)
indexEqns = runWriter . itraverse f . Map.assocs where
  f i (n,e) = tell (Map.singleton n i) $> e

toSSystem :: ([(ODE, VarExpr Double)], Map String Int)
          -> Either String (SSystem NumLearn)
toSSystem (xs,m) =
  SSystem <$> fmap Seq.fromList (traverse (getOdes.fst) xs)
          <*> getInits xs where
    getInits = Right . Seq.fromList . map (Left . snd)
    getOdes (ODE pf pe nf ne) =
      SRow (getConstN pf pe)
           (getConstN nf ne) <$>
           (expList <$> checkMap pe) <*>
           (expList <$> checkMap ne)
    getConstN (Just x) _ = x
    getConstN Nothing fs
      | Map.null fs = Left 0
      | otherwise = Left 1
    checkMap e = (mapM_ f . Map.keys) e $> e where
      f n = case Map.lookup n m of
        Nothing -> Left $ "Unrecognised name: " ++ n
        Just _ -> pure ()
    inOrder = (map fst . sortOn snd . Map.assocs) m
    expList e =
      Seq.fromList $ map (flip (Map.findWithDefault (Left 0)) e) inOrder

fromParsed :: [Either (String, ODE) (String, VarExpr Double)]
           -> Either String (SSystem NumLearn)
fromParsed xs = (toSSystem . indexEqns) =<< matchEqns xs

parseSystem :: String -> Either String (SSystem NumLearn)
parseSystem s = case parseString declarations mempty s of
  Failure f -> Left (show f)
  Success xs -> fromParsed xs

parseSystemFromFile :: String -> IO (Either String (SSystem NumLearn))
parseSystemFromFile f =
  parseFromFileEx declarations f <&> \case
    Failure d -> Left (show d)
    Success x -> fromParsed x

parseTester :: Parser a -> String -> Either String a
parseTester p s = case parseString (whiteSpace *> p <* eof) mempty s of
  Failure d -> Left (show d)
  Success x -> Right x
