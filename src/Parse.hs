{-# LANGUAGE OverloadedStrings #-}

module Parse ( initialAssign
             , diffForm
             , parseTester
             , parseSystem
             , ModelParser
             ) where

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           Data.Functor           (($>))
import qualified Data.List              as List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as Map
import           Data.Maybe             (fromJust)
import           Data.Text              (Text)
import           Data.Tuple             (swap)
import           Expr
import           Prelude                hiding (unlines, (^))
import           SSystem
import           Text.Parsec            hiding (State, uncons, (<|>))
import           Text.Parsec.Expr
import qualified Text.Parsec.Token      as Token
import           Utils

-- | Stateful model type, to be filled as the file is parsed. This allows errors
-- such as duplicate variables to carry a line number and position.
data FillableModel = FM { -- | Initial states of variables
                          fillAss :: Map String InitialDeclaration
                          -- | Power law forms of variables
                        , fillDif :: Map String (PowerLawForm (Either Double AnonParam))
                          -- | Absoulte error tolerance
                        , fillAbs :: Maybe Double
                          -- | Relative error tolerance
                        , fillRel :: Maybe Double
                          -- | Start time
                        , fillStt :: Maybe Double
                          -- | Stop time
                        , fillStp :: Maybe Double
                        , fillSte :: Maybe Double
                        }

-- | This is a parser, which parses 'Text', with a state 'FillableModel'
type ModelParser = Parsec Text FillableModel

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
                                                         , "relTolerance", "ddt", "steps"
                                                         ] ++ funcNames
                                , Token.caseSensitive  = True
                                } where funcNames = map show allFuncs

-- Derived Parsers
lexer                :: Token.GenTokenParser Text st Identity
reservedOp, reserved :: String -> ModelParser ()
parens, bracks       :: ModelParser a -> ModelParser a
semiSep1             :: ModelParser a -> ModelParser [a]
double               :: ModelParser Double
function             :: Func -> Operator Text FillableModel Identity Expr
identifier           :: ModelParser String
whiteSpace           :: ModelParser ()
lexer      = Token.makeTokenParser languageDef
reservedOp = Token.reservedOp lexer
identifier = Token.identifier lexer
parens     = Token.parens     lexer
bracks     = Token.brackets   lexer
reserved   = Token.reserved   lexer
whiteSpace = Token.whiteSpace lexer
semiSep1   = Token.semiSep1   lexer
double     = (try (reservedOp "-" $> negate) <|> pure id)
             <*> (try (Token.float lexer) <|> fromInteger <$> Token.integer lexer)
function f = Prefix $ (reservedOp . show) f $> app f

-- Operator Table
operators :: OperatorTable Text FillableModel Identity Expr
operators =  [ map function allFuncs
             , [Prefix (reservedOp "-" $> negate)]
             , [Infix (reservedOp "^" $> (^)) AssocRight]
             , [Infix (reservedOp "*" $> (*)) AssocLeft ]
             , [Infix (reservedOp "-" $> (-)) AssocLeft ]
             , [Infix (reservedOp "+" $> (+)) AssocLeft ]
             ]

-- Parsers for expressions, for the initial states of variables
expression :: ModelParser Expr
expression = buildExpressionParser operators term

term :: ModelParser Expr
term = parens expression <|> fromDouble <$> double

-- | Parses the initial declaration of a variable, i.e.
-- x = sin 1 + 2
initialAssign :: ModelParser InitialDeclaration
initialAssign = do
  var <- identifier
  reservedOp "="
  expr <- expression
  pure (ID var expr)

-- | Parses an initial declaration, then uses the 'FillableModel' state to check
-- that that variable has not been already declared.
initialAssignF :: ModelParser ()
initialAssignF = do
  d @ ID { idName = var } <- initialAssign
  m @ FM { fillAss = a } <- getState
  let err = "duplicate initial states for variable " ++ var
  maybe (unexpected err) (\n -> putState $ m { fillAss = n }) (insertUnique var d a)

-- Parameter datatype with an unfilled 'name' field
type AnonParam = String -> Parameter

-- | Parses the syntax to declare some exponent as "variable"
-- [1 -> 0..2]
-- Simulated = 1
-- Minimum = 0
-- Maximum = 2
variableExp :: ModelParser AnonParam
variableExp = bracks $ do
  dSim <- double <?> "number representing simulated parameter"
  reservedOp "->"
  dMin <- double <?> "number representing minimum parameter"
  reservedOp ".."
  dMax <- double <?> "number representing maximum parameter"
  pure $ Param dMax dMin dSim

-- | Parses the derivative declaration of a term, i.e:
-- ddt x = y ^ 2 * 3 * z ^ [1->0..2] - w
diffForm :: ModelParser (PowerLawForm (Either Double AnonParam))
diffForm = do
  try (reserved "ddt")
  var <- identifier
  reservedOp "="
  lhs <- prodList <?> "product of power-law terms"
  rhs <- (reservedOp "-" >> prodList) <|> pure []
  pure (PLawF var lhs rhs) where
    prodList = sepBy1 (C <$> double <|> pTerm <?> "power-law term") (reservedOp "*")
    pTerm = do
      var <- identifier
      exv <- try (reservedOp "^" >> eitherA double variableExp) <|> pure (Left 1.0)
      pure $ var :^: exv

-- | Parses the derivative declaration of a term, the 'FillableModel' state to check
-- that that variable has not been already declared.
diffFormF :: ModelParser ()
diffFormF = do
  d @ PLawF { derivOf = var } <- diffForm
  m @ FM { fillDif = a } <- getState
  let err = "duplicate differential equations for variable " ++ var
  maybe (unexpected err) (\n -> putState $ m { fillDif = n }) (insertUnique var d a)

-- | Utility function for making a parser for the simulation parameters
filler :: String
       -> (FillableModel -> Maybe Double)
       -> (Double -> FillableModel -> FillableModel)
       -> ModelParser ()
filler key getter setter = do
  try (reserved key)
  reservedOp "="
  num <- double
  modl <- getState
  case getter modl of
    Just _ -> unexpected ("second " ++ key)
    Nothing -> putState (setter num modl)

absTolF, relTolF, sttTimF, stpTimF, stpSizF :: ModelParser ()
absTolF = filler "absTolerance" fillAbs (\d m -> m { fillAbs = Just d })
relTolF = filler "relTolerance" fillRel (\d m -> m { fillRel = Just d })
sttTimF = filler "start"        fillStt (\d m -> m { fillStt = Just d })
stpTimF = filler "stop"         fillStp (\d m -> m { fillStp = Just d })
stpSizF = filler "steps"        fillSte (\d m -> m { fillSte = Just d })

parseSystem :: String -> Text -> Either ParseError (Configurable (Either Double Parameter))
parseSystem = flip runParser emptyFill $ do
  whiteSpace
  _ <- semiSep1 (choice [ absTolF, relTolF, sttTimF, stpTimF
                        , stpSizF, diffFormF, initialAssignF])
  modl <- getState
  case modl of
    FM { fillAbs = Nothing } -> unexpected "eof. Expecting absolute tolerance"
    FM { fillRel = Nothing } -> unexpected "eof. Expecting relative tolerance"
    FM { fillStt = Nothing } -> unexpected "eof. Expecting start time"
    FM { fillStp = Nothing } -> unexpected "eof. Expecting stop time"
    FM { fillSte = Nothing } -> unexpected "eof. Expecting number of steps"
    FM a d (Just b) (Just r) (Just s) (Just e) (Just t) -> do
      let dm = ("differential equation missing for: " ++)
      let im = ("initial equation missing for: " ++)
      let w = symmetricDifferenceWith  (dm . idName) (im . derivOf) a d
      if null w then
        pure $ Configurable (SSystem (Map.elems a) ((unAnon . Map.elems) d)) s e b r t
      else (unexpected . ("eof. " ++) . List.unlines . Map.elems) w

-- Utility

unAnon :: [PowerLawForm (Either Double AnonParam)] -> [PowerLawForm (Either Double Parameter)]
unAnon = evalState uniqNames . (traverse . traverse . traverse) (<$> uniqName)

eitherA :: Alternative f => f a -> f b -> f (Either a b)
eitherA x y = Left <$> x <|> Right <$> y


allFuncs :: [Func]
allFuncs = [minBound..maxBound]

parseTester :: (Show a, Eq a) => ModelParser a -> a -> Text -> Maybe String
parseTester p e s = either (Just . show) f (runParser p emptyFill "testing" s) where
  f r | r == e = Nothing
      | otherwise = Just $ "Expected: " ++ show e ++ "\n" ++ "Received: " ++ show r

emptyFill :: FillableModel
emptyFill = FM Map.empty Map.empty Nothing Nothing Nothing Nothing Nothing
