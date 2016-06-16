{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Parse
       ( parseSystem
       , parseTester
       , numLearn
       , ode
       , listOf
       , double'
       ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.Sequence               (Seq)
import qualified Data.Sequence               as Seq
import           Data.Text                   (Text, unpack)
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

data ODE =
  ODE { _posFac :: Maybe NumLearn
      , _posExp :: Map String NumLearn
      , _negFac :: Maybe NumLearn
      , _negExp :: Map String NumLearn}

instance Show ODE where show (ODE a b c d) = show (a,b,c,d)

data ParseState =
  ParseState { _odes     :: Map String ODE
             , _initials :: Map String (Expr Double)
             }

makeLenses ''ParseState

beginState :: ParseState
beginState = ParseState Map.empty Map.empty

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
    unenum [x,y] = symbol ".." *> fmap (enumFromThenTo x y) parser <|> pure [x,y]
    unenum xs = pure xs

-- | Parses the syntax for declaring a parameter.
--
-- >>> parseTester numLearn "[1,2,3]"
-- Right (Right [1.0,2.0,3.0])
numLearn :: (TokenParsing m, Monad m) => m NumLearn
numLearn = eitherA double' (listOf double')

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

odeTup :: (Maybe NumLearn, Map String NumLearn)
       -> (Maybe NumLearn, Map String NumLearn)
       -> ODE
odeTup (a,b) (c,d) = ODE a b c d

-- | Parses an ode
-- >>> parseTester ode "2 * x1 ^ 3"
-- Right (Just (Left 2.0),fromList [("x1",Left 3.0)],Nothing,fromList [])
-- >>> parseTester ode "-4.5 * x4 ^ 2"
-- Right (Nothing,fromList [],Just (Left 4.5),fromList [("x4",Left 2.0)])
ode :: (TokenParsing m, Monad m) => m ODE
ode = odeTup <$> (poss <|> sidel) <*> (negs <|> sidel) where
  facs = (,) <$> optional numLearn
             <*  optional (symbol "*")
             <*> termList
  poss = notFollowedBy (symbol "-") *> facs
  sidel = pure (Nothing, mempty)
  negs = symbol "-" *> facs
  termList = Map.fromList <$> sepBy term (symbol "*")
  term = (,) <$> ident identStyle <*> ((symbol "^" *> numLearn) <|> pure (Left 1.0))

parseOde :: (Monad m, TokenParsing m) => m (String, ODE)
parseOde = (,) <$> (reserve identStyle "ddt" *> ident identStyle) <*> (symbol "=" *> ode)

-- | Parses the declaration of the initial state of a variable
parseInitialValue :: (Monad m, TokenParsing m) => m (String, Expr Double)
parseInitialValue = (,) <$> ident identStyle <*> (symbol "=" *> exprParse)

type FillState a = StateT (Seq (Seq (NumLearn,NumLearn))) (Either String) a

fillSSystem :: Map String (ODE, Expr Double) -> FillState [STerm NumLearn]
fillSSystem m = itraverse fill vals where
  vals = Map.toList m
  idxs = Map.fromList (imap (\i (n,_) -> (n,i)) vals)
  fill i (n, (ODE pf_ pe_ nf_ ne_, e)) = do
    updSquare pe_ i _1
    updSquare ne_ i _2
    let pf = fromMaybe (Left $ bool 0 1 (Map.null pe_)) pf_
    let nf = fromMaybe (Left $ bool 0 1 (Map.null ne_)) nf_
    iv <- lift . maybe (Left "Divide by zero") Right $ safeEval e
    pure $ STerm pf nf (Left iv) n
  updSquare :: Map String NumLearn
            -> Int
            -> Lens' (NumLearn,NumLearn) NumLearn
            -> FillState ()
  updSquare p i s = forM_ (Map.toList p) $ \(en,ev) -> do
      j <- maybe (lift . Left . err' $ en) pure (Map.lookup en idxs)
      ix i . ix j . s .= ev
  err' :: String -> String
  err' en = "Variable without ode: " ++ en

toSSystem :: ParseState -> Either String (SSystem NumLearn)
toSSystem (ParseState o i) = do
  let err' xs = "Equations not matched: " ++ show xs
  m <- over _Left err' $ mergeMatch (,) o i
  let n = Map.size m
  let s = Seq.replicate n (Seq.replicate n (Left 0, Left 0))
  (trms,exps') <- runStateT (fillSSystem m) s
  pure $ SSystem exps' trms

parseLines :: (Monad m, TokenParsing m) => m [Either (String, ODE) (String, Expr Double)]
parseLines =
  whiteSpace *>
  semiSep1 (eitherA parseOde parseInitialValue) <*
  optional semi <*
  eof

parseSystem :: Text -> Either String (SSystem NumLearn)
parseSystem s =
  toSSystem =<<
  flip execStateT beginState .
  traverse (either (upd odes) (upd initials)) =<<
  case parseString parseLines mempty (unpack s) of
    Failure d -> Left (show d)
    Success x -> Right x
  where
      upd :: Lens' ParseState (Map String a)
          -> (String, a)
          -> StateT ParseState (Either String) ()
      upd l (n,v) = do
        seen <- uses (l . at n) isJust
        when seen . lift . Left $ "Duplicate equations for " ++ n
        l . at n ?= v

parseTester :: Parser a -> String -> Either String a
parseTester p s = case parseString (whiteSpace *> p <* eof) mempty s of
  Failure d -> Left (show d)
  Success x -> Right x
