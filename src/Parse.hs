{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Parse
       ( Parser
       , ODE(..)
       , InitialDeclaration(..)
       , parseTester
       , parseSystem
       ) where

import           Control.Applicative        hiding (optional)
import           Control.Arrow              ((***))
import           Control.Monad.Identity     (Identity)
import           Control.Monad.State
import           Data.Either                (partitionEithers)
import           Data.Foldable
import           Data.Functor               (($>))
import           Data.List                  (sortOn, uncons)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Expr
import           Prelude                    hiding (unlines, (^))
import           SSystem
import           Text.Parsec                hiding (State, many, uncons, (<|>))
import           Text.Parsec.Expr
import qualified Text.Parsec.Token          as Token
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
semiSep1, commaSep   :: Parser a -> Parser [a]
function             :: Func -> Operator Text () Identity Expr
identifier           :: Parser String
whiteSpace, star, carat, hyph :: Parser ()
pnegate              :: Num a => Parser (a -> a)
double               :: Parser Double
integer              :: Parser Integer
lexer      = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
parens     = Token.parens     lexer
bracks     = Token.brackets   lexer
reserved   = Token.reserved   lexer
whiteSpace = Token.whiteSpace lexer
semiSep1   = Token.semiSep1   lexer
commaSep   = Token.commaSep   lexer
double     = Token.float      lexer
integer    = Token.integer    lexer
pnegate    = reservedOp "-" $> negate <|> pure id
star       = reservedOp "*"
carat      = reservedOp "^"
hyph       = reservedOp "-"

instance Parse Double where
  parser = pnegate <*> (try double <|> fromInteger <$> integer)

function f = Prefix $ (reservedOp . show) f $> app f

-- Operator Table
operators :: OperatorTable Text () Identity Expr
operators =  [ map function allFuncs
             , [Prefix (reservedOp "-" $> negate)]
             , [Infix  (reservedOp "^" $> (^)) AssocRight]
             , [Infix  (reservedOp "*" $> (*)) AssocLeft ]
             , [Infix  (reservedOp "-" $> (-)) AssocLeft ]
             , [Infix  (reservedOp "+" $> (+)) AssocLeft ]
             ]

term :: Parser Expr
term = parens parser <|> fromDouble <$> parser

instance Parse Expr where
  parser = buildExpressionParser operators term

instance Parse NumLearn where
  parser = fmap NumLearn (fmap Right parser <|> fmap Left parser)

instance Parse InitialDeclaration where
  parser = ID <$> identifier <* reservedOp "=" <*> parser

instance (Parse a, Enum a) => Parse [a] where
    parser = bracks $ f =<< commaSep parser where
      f [x] = reservedOp ".." *> fmap (enumFromTo x) parser <|> pure [x]
      f [x,y] = reservedOp ".." *> fmap (enumFromThenTo x y) parser <|> pure [x,y]
      f xs = pure xs

instance Parse ODE where
  parser = liftA2 uncurry (liftA2 uncurry (fmap ODE var) lhs) rhs where
    lhs = notFollowedBy hyph *> lst <|> emt
    rhs = hyph *> lst <|> emt
    var = try (reserved "ddt") *> identifier <* reservedOp "="
    fac = (parser <* optional star) <|> static 1
    emt = (,) <$> static 0 <*> pure []
    lst = (,) <$> fac <*> (sepBy trm star >>= uni)
    uni = fmap Map.assocs . foldrM (\(t,e) -> maybe (unexpected t) pure . insertUnique t e) Map.empty
    trm = (,) <$> identifier <*> (carat *> parser <|> static 1)

static :: Applicative f => Double -> f NumLearn
static = pure . NumLearn . Left

data InitialDeclaration = ID { idName :: String
                             , idVal  :: NumLearn }

data ODE = ODE { odeName   :: String
               , odePosFac :: NumLearn
               , odePosExp :: [(String,NumLearn)]
               , odeNegFac :: NumLearn
               , odeNegExp :: [(String, NumLearn)]
               } deriving (Eq, Show)

parseSystem :: String -> Text -> Either String (SSystem NumLearn)
parseSystem s t = fromParsed =<< mapLeft show (runParser (whiteSpace *> semiSep1 (eitherA parser parser)) () s t)

-- Utility

allFuncs :: [Func]
allFuncs = [minBound..maxBound]

parseTester :: (Show a, Eq a) => Parser a -> a -> Text -> Maybe String
parseTester p e s = either (Just . show) f (runParser p () "testing" s) where
  f r | r == e = Nothing
      | otherwise = Just $ "Expected: " ++ show e ++ "\n" ++ "Received: " ++ show r


fromParsed :: [Either InitialDeclaration ODE] -> Either String (SSystem NumLearn)
fromParsed = fmap SSystem . uncurry f . (sortOn idName *** sortOn odeName) . partitionEithers where
  prepZero x = NumLearn (Left 0.0) : x
  nextVar = StateT (maybe (Left "ODE too long") Right . uncons)
  f i = zipWithM g i where
    vars = map idName i
    g (ID b v) (ODE a pf pe nf ne)
      | a == b = STerm pf nf <$> h pe <*> h ne <*> pure v
      | otherwise = (Left . unwords) ["Variables", a, "and", b, "mismatched."]
    h = fmap concat . flip evalStateT vars . traverse (uncurry j)
    j w n = go where go = nextVar >>= bool (pure [n]) (fmap prepZero go) . (w==)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right x) = Right x
