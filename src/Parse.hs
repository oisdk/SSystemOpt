{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
module Parse
       ( parseSystem
       , parseTester
       , expr
       , numLearn
       , ode
       , Parser
       ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Functor
import           Data.List
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe
import           Data.Ord
import           Data.Text           (Text, pack)
import           Expr
import           Prelude             hiding (unlines)
import           Square
import           SSystem
import           Text.Parsec         hiding (State, many, optional, uncons,
                                      (<|>))
import           Text.Parsec.Expr
import           Text.Parsec.Text
import qualified Text.Parsec.Token   as Token
import           Utils

data ODE =
  ODE { _posFac :: Maybe NumLearn
      , _posExp :: Map String NumLearn
      , _negFac :: Maybe NumLearn
      , _negExp :: Map String NumLearn
      } deriving Show

data ParseState =
  ParseState { _odes     :: Map String ODE
             , _initials :: Map String Expr
             }

makeLenses ''ParseState

beginState :: ParseState
beginState = ParseState Map.empty Map.empty

-- | Language Definition
languageDef :: Token.GenLanguageDef Text () Identity
languageDef =
  Token.LanguageDef
    { Token.commentStart   = ""
    , Token.commentEnd     = ""
    , Token.commentLine    = "//"
    , Token.nestedComments = True
    , Token.identStart     = letter
    , Token.identLetter    = alphaNum
    , Token.opStart        = Token.opLetter languageDef
    , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
    , Token.reservedOpNames= [ "+", "*", "=", "-", "^", "->", "..", "/"
                              ] ++ funcNames
    , Token.reservedNames  = "ddt" : funcNames
    , Token.caseSensitive  = True
    } where funcNames = map show allFuncs

-- Derived Parsers
lexer :: Token.GenTokenParser Text () Identity
reservedOp, reserved :: String -> Parser ()
semiSep1 :: Parser a -> Parser [a]
function :: Func -> Operator Text () Identity Expr
identifier :: Parser String
whiteSpace, star, carat, hyph, eqsn, dots :: Parser ()
double :: Parser Double
lexer = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
whiteSpace = Token.whiteSpace lexer
semiSep1 = Token.semiSep1 lexer
star = reservedOp "*"; carat = reservedOp "^"
hyph = reservedOp "-"; eqsn = reservedOp "="
dots = reservedOp ".."
double = (hyph *> (negate <$> num)) <|> num where
  num = try (Token.float lexer) <|> fromInteger <$> Token.integer lexer
function f = Prefix $ (try . reservedOp . show) f $> (:$:) f

-- Operator Table
operators :: OperatorTable Text () Identity Expr
operators =  [ map function (sortOn (Down . length . show) allFuncs)
             , [Prefix (reservedOp "-" $> negate)]
             , [Infix  (reservedOp "^" $> (**)) AssocRight]
             , [Infix  (reservedOp "/" $> (/) ) AssocRight]
             , [Infix  (reservedOp "*" $> (*) ) AssocLeft ]
             , [Infix  (reservedOp "-" $> (-) ) AssocLeft ]
             , [Infix  (reservedOp "+" $> (+) ) AssocLeft ] ]

term :: Parser Expr
term = Token.parens lexer expr <|> fmap fromDouble double

expr :: Parser Expr
expr = buildExpressionParser operators term

listOf :: Enum a => Parser a -> Parser [a]
listOf parser =
  Token.brackets lexer (unenum =<< Token.commaSep lexer parser) where
    unenum [x] = dots *> fmap (enumFromTo x) parser <|> pure [x]
    unenum [x,y] = dots *> fmap (enumFromThenTo x y) parser <|> pure [x,y]
    unenum xs = pure xs

numLearn :: Parser NumLearn
numLearn = eitherA double (listOf double)

odeTup :: (Maybe NumLearn, Map String NumLearn)
       -> (Maybe NumLearn, Map String NumLearn)
       -> ODE
odeTup (a,b) (c,d) = ODE a b c d

ode :: Parser ODE
ode = odeTup <$> side
             <*> (hyph *> side <|> emptySide) where
  term' = (,) <$> identifier <*> defExp
  defExp = (carat *> numLearn) <|> pure (Left 1)
  side = (,) <$> fmap Just numLearn
             <*> (Map.fromList <$> many (star *> term'))
             <|> ((,) Nothing . Map.fromList)
             <$> sepBy term' star
  emptySide = pure (Nothing, Map.empty)

parseOde :: Parser (String, ODE)
parseOde = (,) <$> (reserved "ddt" *> identifier) <*> (eqsn *> ode)

parseInitialValue :: Parser (String, Expr)
parseInitialValue = (,) <$> identifier <*> (eqsn *> expr)

type FillState a = StateT (Square (NumLearn,NumLearn)) (Either String) a

fillSSystem :: Map String (ODE, Expr) -> FillState [STerm NumLearn]
fillSSystem m = itraverse fill vals where
  vals = Map.toList m
  idxs = Map.fromList (imap (\i (n,_) -> (n,i)) vals)
  fill i (n, (ODE pf_ pe_ nf_ ne_, e)) = do
    updSquare pe_ i _1
    updSquare ne_ i _2
    let pf = fromMaybe (Left $ bool 0 1 (Map.null pe_)) pf_
    let nf = fromMaybe (Left $ bool 0 1 (Map.null ne_)) nf_
    iv <- lift $ safeEval e
    pure $ STerm pf nf (Left iv) n
  updSquare :: Map String NumLearn
            -> Int
            -> Lens' (NumLearn,NumLearn) NumLearn
            -> FillState ()
  updSquare p i s = forM_ (Map.toList p) $ \(en,ev) -> do
      j <- maybe (lift . Left . err $ en) pure (Map.lookup en idxs)
      ix (i,j) . s .= ev
  err :: String -> String
  err en = "Variable without ode: " ++ en

toSSystem :: ParseState -> Either String (SSystem NumLearn)
toSSystem (ParseState o i) = do
  let err xs = "Equations not matched: " ++ show xs
  m <- over _Left err $ mergeMatch (,) o i
  let n = Map.size m
  let s = runIdentity $ create n (Identity (Left 0, Left 0))
  (trms,exps) <- runStateT (fillSSystem m) s
  pure $ SSystem exps trms

parseLines :: Parser [Either (String, ODE) (String, Expr)]
parseLines = whiteSpace *> semiSep1 (eitherA parseOde parseInitialValue)

parseSystem :: String -> Text -> Either String (SSystem NumLearn)
parseSystem s =
  toSSystem <=<
  flip execStateT beginState .
  traverse (either (upd odes) (upd initials)) <=<
  over _Left show . parse parseLines s where
    upd :: Lens' ParseState (Map String a)
        -> (String, a)
        -> StateT ParseState (Either String) ()
    upd l (n,v) = do
      seen <- uses (l . at n) isJust
      when seen . lift . Left $ "Duplicate equations for " ++ n
      l . at n ?= v

-- Utility

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

parseTester :: Parser a -> String -> Either String a
parseTester p =
  over _Left show . parse (whiteSpace *> p <* eof) "testing" . pack
