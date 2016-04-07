{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Parse where

import           Control.Applicative
import           Control.Arrow          ((***))
import           Control.Monad.Identity (Identity)
import           Control.Monad.State
import           Data.Either            (partitionEithers)
import           Data.Functor           (($>))
import qualified Data.List              as List
import           Data.Text              (Text)
import           Expr
import           Prelude                hiding (unlines, (^))
import           SSystem
import           Text.Parsec            hiding (State, uncons, (<|>))
import           Text.Parsec.Expr
import qualified Text.Parsec.Token      as Token
import           Utils

type Parser = Parsec Text ()

class Parse a where
  parser :: Parser a

-- | Language Definition
languageDef :: Token.GenLanguageDef Text st Identity
languageDef = Token.LanguageDef { Token.commentStart   = ""
                                , Token.commentEnd     = ""
                                , Token.commentLine    = "//"
                                , Token.nestedComments = True
                                , Token.identStart     = letter
                                , Token.identLetter    = alphaNum
                                , Token.opStart        = Token.opLetter languageDef
                                , Token.opLetter       = oneOf ":!#$%&*+./<=>?@\\^|-~"
                                , Token.reservedOpNames= [ "+", "*", "=", "-", "^", "->", ".."
                                                         ] ++ funcNames
                                , Token.reservedNames  = [ "start", "stop", "absTolerance"
                                                         , "relTolerance", "ddt", "steps", "stepsize"
                                                         ] ++ funcNames
                                , Token.caseSensitive  = True
                                } where funcNames = map show allFuncs

-- Derived Parsers
lexer                :: Token.GenTokenParser Text st Identity
reservedOp, reserved :: String -> Parser ()
parens, bracks       :: Parser a -> Parser a
semiSep1             :: Parser a -> Parser [a]
function             :: Func -> Operator Text () Identity Expr
identifier           :: Parser String
whiteSpace           :: Parser ()
lexer      = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
parens     = Token.parens     lexer
bracks     = Token.brackets   lexer
reserved   = Token.reserved   lexer
whiteSpace = Token.whiteSpace lexer
semiSep1   = Token.semiSep1   lexer

instance Parse Double where
  parser = (negate <$ reservedOp "-" ?? id)
           <*> (try (Token.float lexer)
           <|> fromInteger <$> Token.integer lexer)

function f = Prefix $ (reservedOp . show) f $> app f

-- Operator Table
operators :: OperatorTable Text () Identity Expr
operators =  [ map function allFuncs
             , [Prefix (reservedOp "-" $> negate)]
             , [Infix (reservedOp "^" $> (^)) AssocRight]
             , [Infix (reservedOp "*" $> (*)) AssocLeft ]
             , [Infix (reservedOp "-" $> (-)) AssocLeft ]
             , [Infix (reservedOp "+" $> (+)) AssocLeft ]
             ]

term :: Parser Expr
term = parens parser <|> fromDouble <$> parser

instance Parse Expr where
  parser = buildExpressionParser operators term

newtype NumLearn = NumLearn { getNumLearn :: Either Double (Parameter Double) }

instance Parse NumLearn where
  parser = NumLearn <$> eitherA (fmap eval (parser :: Parser Expr)) parser

instance Parse InitialDeclaration where
  parser = ID <$> identifier <* reservedOp "=" <*> parser

instance (Parse a, Enum a, Fractional a) => Parse (Parameter a) where
    parser = Parameter <$> parser
                       <*  reservedOp ".."
                       <*> (enumFromThenTo <$> parser
                                           <*> pure 0.5
                                           <*  reservedOp "->"
                                           <*> parser)

instance Parse ODE where
  parser = do
    try (reserved "ddt")
    var <- identifier
    reservedOp "="
    (lfc,lhs) <- pList ?? (nlz,[])
    (rfc,rhs) <- (reservedOp "-" *> pList) ?? (nlz,[])
    pure (ODE var lfc lhs rfc rhs) where
      pList = (,) <$> parser <*> sepBy pTerm (reservedOp "*") <|>
              (nlo,) <$> sepBy1 pTerm (reservedOp "*")
      pTerm = (,) <$> identifier
                  <*> ((reservedOp "^" *> parser) ?? nlo)
nlz, nlo :: NumLearn
nlz = NumLearn (Left 0)
nlo = NumLearn (Left 1)

data InitialDeclaration = ID { idName :: String
                             , idVal  :: NumLearn }

data ODE = ODE { odeName   :: String
               , odePosFac :: NumLearn
               , odePosExp :: [(String,NumLearn)]
               , odeNegFac :: NumLearn
               , odeNegExp :: [(String, NumLearn)]
               }

parseSystem :: String -> Text -> Either ParseError [Either InitialDeclaration ODE]
parseSystem = runParser (whiteSpace *> semiSep1 (eitherA parser parser)) ()

-- Utility

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

-- parseTester :: (Show a, Eq a) => Parser a -> a -> Text -> Maybe String
-- parseTester p e s = either (Just . show) f (runParser p emptyFill "testing" s) where
--   f r | r == e = Nothing
--       | otherwise = Just $ "Expected: " ++ show e ++ "\n" ++ "Received: " ++ show r


fromParsed :: [Either InitialDeclaration ODE] -> Either String (SSystem NumLearn)
fromParsed = f . (List.sortOn idName *** List.sortOn odeName) . partitionEithers where
  f (i,o) = SSystem <$> zipWithM g o i where
    vars = map idName i
    g ODE{odeName=a} (ID b _) | a /= b = Left $ "Variables " ++ a ++ " and " ++ b ++ " mismatched."
    g (ODE _ pf pe nf ne) (ID _ i) = STerm pf nf <$> h pe <*> h ne <*> pure i
    h = fmap concat . flip evalStateT vars . traverse j
    j i@(w,n) = do
      x <- StateT (maybe (Left "ODE too long") Right . List.uncons)
      if w == x then pure [n] else fmap (nlo:) (j i)
