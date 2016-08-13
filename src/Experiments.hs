{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Experiments
  ( Experiment(..)
  , Network(..)
  , VariableData(..)
  , VariableDataPoint(..)
  , toExpFormat
  , problem
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Data.IntMap.Strict          (IntMap)
import qualified Data.IntMap.Strict          as IntMap
import           Data.List                   hiding (zipWith)
import qualified Data.Sequence               as Seq
import           Data.Text.Lazy              (Text)
import qualified Data.Text.Lazy              as Text
import           GHC.Exts                    (fromList)
import           Numeric.Expr
import           Prelude                     hiding (zipWith)
import           SSystem
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Highlight
import           Text.Parser.Token.Style
import           Text.Trifecta.Parser
import           Utils

-- | A Parser for correctly handling the problems on
-- http://www.cse.chalmers.se/~dag/identification/Benchmarks/Problems.html
newtype ExperimentParser a = ExperimentParser
  { getExprParser :: Parser a
  } deriving ( Functor, Applicative, Monad, Alternative
             , Parsing, CharParsing, MonadPlus)

-- | Special whitespace handling for comments
instance TokenParsing ExperimentParser where
  someSpace = ExperimentParser $
    buildSomeSpaceParser
      someSpace
      (CommentStyle "" "" "//" False)

identStyle :: IdentifierStyle ExperimentParser
identStyle =
  IdentifierStyle
    "variable"
    letter
    (alphaNum <|> char '_')
    (fromList
      [ "has", "of", "sample_", "experiment_", "time", "variable_"
      , "sdev", "=", "lowerBound", "defaultUpperBound"
      , "defaultLowerBound", "dependent", "inputVariable", "name", "type"
      , "date", "url", "begin", "problem", "format", "version", "is"
      , "upperBound", "errorFunction", "equation", "lambda", "alpha"
      , "beta", "g", "h"])
    Identifier
    ReservedIdentifier

identifier :: ExperimentParser String
identifier = ident identStyle
reserved :: String -> ExperimentParser ()
reserved = reserve identStyle

-- | Parses syntax for declaring a property
-- >>> parseTest (getExprParser (prop "size")) "has  size = "
-- ()
prop :: String -> ExperimentParser ()
prop n = reserved "has" *> reserved n *> reserved "="

data Sample = Sample
  { _sampleNum           :: Int
  , _sampleExperimentNum :: Int
  , _sampleTime          ::  Double
  , _sampleVars          :: [Double]
  , _sampleSdev          :: [Double]
  } deriving Show

makeFields ''Sample

posInt :: ExperimentParser Int
posInt = fmap fromInteger natural

-- |
-- >>> :{
-- let sampleStr = unlines
--       [ "sample_22 of experiment_1                                                                                        "
--       , "has time =  0.42000000E+01                                                                                       "
--       , "has variable_ =   0.1908074706767553E+01  0.3267195980257653E+01  0.5606229424488210E+00  0.7500000000000000E+00 "
--       , "has sdev of variable_ =  0.10000000E-01 0.10000000E-01 0.10000000E-01 0.00000000E+00                             "]
-- in parseTest (getExprParser (sample 4)) sampleStr
-- :}
-- Sample {_sampleNum = 22, _sampleExperimentNum = 1, _sampleTime = 4.2, _sampleVars = [1.908074706767553,3.267195980257653,0.560622942448821,0.75], _sampleSdev = [1.0e-2,1.0e-2,1.0e-2,0.0]}
sample :: Int -> ExperimentParser Sample
sample n =
  Sample <$> (string "sample_" *>
              posInt)
         <*> (reserved "of" *>
              string "experiment_" *>
              posInt)
         <*> (prop "time" *>
              double)
         <*> (prop "variable_" *>
              count n double)
         <*> (reserved "has" *>
              reserved "sdev" *>
              reserved "of" *>
              reserved "variable_" *>
              reserved "=" *>
              count n double)

data Variable = Variable
  { _variableNum  :: Int
  , _variableName :: String
  , _variableDep  :: Dependency
  } deriving Show

data Dependency = Input | Dependent deriving Show

makeFields ''Variable

dependency :: ExperimentParser Dependency
dependency = Dependent <$ reserved "dependent"
         <|> Input     <$ reserved "inputVariable"

-- |
-- >>> parseTest (getExprParser varDecl) "variable_2 has name = x2 is dependent"
-- Variable {_variableNum = 2, _variableName = "x2", _variableDep = Dependent}
varDecl :: ExperimentParser Variable
varDecl =
  Variable <$> (string "variable_" *> posInt)
           <*> (prop "name" *> identifier)
           <*> (reserved "is" *> dependency)

data ExprDecl = ExprDecl
  { _exprDeclNum    :: Int
  , _exprDeclName   :: String
  , _exprDeclDatVal :: DataVal
  } deriving Show

data DataVal = Perfect | Imperfect deriving Show

makeFields ''ExprDecl

dataVal :: ExperimentParser DataVal
dataVal = Perfect   <$ reserved "perfectData"
      <|> Imperfect <$ reserved "imperfectData"

-- |
-- >>> parseTest (getExprParser exprDecl) "experiment_1 has name = exp1 has perfectData"
-- ExprDecl {_exprDeclNum = 1, _exprDeclName = "exp1", _exprDeclDatVal = Perfect}
exprDecl :: ExperimentParser ExprDecl
exprDecl =
  ExprDecl <$> (string "experiment_" *> posInt)
           <*> (prop "name" *> identifier)
           <*> (reserved "has" *> dataVal)

-- |
-- >>> parseTest (getExprParser bound) "0."
-- 0.0
-- >>> parseTest (getExprParser bound) "-20."
-- -20.0
bound :: ExperimentParser Double
bound = (try double <|> fmap fromInteger integer) <*
        token (skipOptional (char '.'))

-- |
-- >>> parseTest (getExprParser $ string "alpha" *> ind) "alpha_1"
-- 1
ind :: ExperimentParser Int
ind = char '_' *> posInt

data FilledBound =
  NoBound |
  JustLower Double |
  JustUpper Double |
  Both Double Double
  deriving (Show, Eq)

makePrisms ''FilledBound

data BoundFilling = BoundFilling
  { _boundFillingAlpha  :: FilledBound
  , _boundFillingBeta   :: FilledBound
  , _boundFillingG      :: FilledBound
  , _boundFillingH      :: FilledBound
  , _boundFillingAlphas :: [FilledBound]
  , _boundFillingBetas  :: [FilledBound]
  , _boundFillingGs     :: [[FilledBound]]
  , _boundFillingHs     :: [[FilledBound]]
  } deriving Show

makeFields ''BoundFilling

addLower :: Double -> FilledBound -> Either String FilledBound
addLower x NoBound = Right (JustLower x)
addLower x (JustUpper u) = Right (Both x u)
addLower _ _ = Left "lower bound"

addUpper :: Double -> FilledBound -> Either String FilledBound
addUpper x NoBound = Right (JustUpper x)
addUpper x (JustLower l) = Right (Both l x)
addUpper _ _ = Left "upper bound"

eitherToP :: (a -> String) -> Either a b -> ExperimentParser b
eitherToP e = either (unexpected . e) pure

eitherToPT :: MonadTrans t => (a -> String) -> Either a b -> t ExperimentParser b
eitherToPT e = lift . eitherToP e

fillDef :: String
        -> Lens' BoundFilling FilledBound
        -> StateT BoundFilling ExperimentParser ()
fillDef nme lns = do
  lift someSpace
  adder <- lift (getBound "defaultLowerBound" "defaultUpperBound")
  res <- uses lns adder
  r <- eitherToPT (\e -> "Duplicate default " ++ e ++ " for " ++ nme) res
  lns .= r

getBound :: String -> String
         -> ExperimentParser (FilledBound -> Either String FilledBound)
getBound lb ub =
  (reserved "has" *>
  ((addLower <$ reserved lb) <|> (addUpper <$ reserved ub))) <*>
  (reserved "=" *> bound)

maybeToP :: String -> Maybe a -> ExperimentParser a
maybeToP e = maybe (unexpected e) pure

maybeToPT :: MonadTrans t => String -> Maybe a -> t ExperimentParser a
maybeToPT e = lift . maybeToP e

fillOneDim :: String
           -> Lens' BoundFilling [FilledBound]
           -> StateT BoundFilling ExperimentParser ()
fillOneDim nme lns = do
  i <- lift ind
  adder <- lift (getBound "lowerBound" "upperBound")
  elm <- preuse (lns . ix (i-1))
  res <- maybeToPT ("Index out of bounds: " ++ show i) elm
  r <- eitherToPT (\e -> concat ["Duplicate ", e, " for ", nme, "_", show i]) (adder res)
  lns . ix (i-1) .= r

fillTwoDim :: String
           -> Lens' BoundFilling [[FilledBound]]
           -> StateT BoundFilling ExperimentParser ()
fillTwoDim nme lns = do
  x <- lift ind
  y <- lift ind
  adder <- lift (getBound "lowerBound" "upperBound")
  elm <- preuse (lns . ix (y-1) . ix (x-1))
  res <- maybeToPT ("Index out of bounds: " ++ show (x,y)) elm
  r <- eitherToPT (\e ->  concat ["Duplicate ", e, " for ", nme, "_", show x, "_", show y]) (adder res)
  lns . ix (y-1) . ix (x-1) .= r

fillBounds :: StateT BoundFilling ExperimentParser ()
fillBounds = choice
  [ lift (string "alpha") *> (fillOneDim "alpha" alphas <|> fillDef "alpha" alpha)
  , lift (string "beta" ) *> (fillOneDim "beta" betas   <|> fillDef "beta" beta)
  , lift (string "g"    ) *> (fillTwoDim "g" gs         <|> fillDef "g" g)
  , lift (string "h"    ) *> (fillTwoDim "h" hs         <|> fillDef "h" h) ]

-- |
-- >>> :{
-- let sampleStr = unlines
--         [ "alpha has defaultUpperBound =  0."
--         , " "
--         , " alpha has defaultLowerBound =  20."
--         , " "
--         , " beta has defaultLowerBound =  0."
--         , " "
--         , " beta has defaultUpperBound =  20."
--         , " "
--         , " g has defaultLowerBound =  -4."
--         , " g_1_1 has lowerBound =  0."
--         , " g_2_2 has lowerBound =  0."
--         , " g_3_3 has lowerBound =  0."
--         , " "
--         , " g has defaultUpperBound =   4."
--         , " g_1_1 has upperBound =  0."
--         , " g_2_2 has upperBound =  0."
--         , " g_3_3 has upperBound =  0."
--         , " "
--         , " h has defaultLowerBound =  -4."
--         , " h_1_1 has lowerBound =  1.0E-15"
--         , " h_2_2 has lowerBound =  1.0E-15"
--         , " h_3_3 has lowerBound =  1.0E-15"
--         , " "
--         , " h has defaultUpperBound =   4." ]
-- in parseTest (getExprParser (boundFill 4)) sampleStr
-- :}
-- BoundFilling {_boundFillingAlpha = Both 20.0 0.0, _boundFillingBeta = Both 0.0 20.0, _boundFillingG = Both (-4.0) 4.0, _boundFillingH = Both (-4.0) 4.0, _boundFillingAlphas = [NoBound,NoBound,NoBound,NoBound], _boundFillingBetas = [NoBound,NoBound,NoBound,NoBound], _boundFillingGs = [[Both 0.0 0.0,NoBound,NoBound,NoBound],[NoBound,Both 0.0 0.0,NoBound,NoBound],[NoBound,NoBound,Both 0.0 0.0,NoBound],[NoBound,NoBound,NoBound,NoBound]], _boundFillingHs = [[JustLower 1.0e-15,NoBound,NoBound,NoBound],[NoBound,JustLower 1.0e-15,NoBound,NoBound],[NoBound,NoBound,JustLower 1.0e-15,NoBound],[NoBound,NoBound,NoBound,NoBound]]}
boundFill :: Int -> ExperimentParser BoundFilling
boundFill = execStateT (some fillBounds) . fromSize where
  fromSize n = BoundFilling
    NoBound
    NoBound
    NoBound
    NoBound
    (replicate n NoBound)
    (replicate n NoBound)
    (replicate n (replicate n NoBound))
    (replicate n (replicate n NoBound))

data Bounds = Bounds
  { _boundsAlphas :: [(Double,Double)]
  , _boundsBetas  :: [(Double,Double)]
  , _boundsHs     :: [[(Double,Double)]]
  , _boundsGs     :: [[(Double,Double)]]
  } deriving Show

makeFields ''Bounds

toSSystem :: Bounds -> SSystem (Either (VarExpr Double) (Double,Double))
toSSystem (Bounds as bs hhs ggs) = SSystem (fromList $ zipWith5 f [0..] as bs hhs ggs) (Seq.replicate (length as) (Left 0)) where
  f i a b hh gg = SRow (Right a) (Right b) (fromList . repl $ hh) (fromList . repl $ gg) where
    repl = imap (\j -> if j == i then const (Left 0) else Right)

getBounds :: BoundFilling -> ExperimentParser Bounds
getBounds bf =
  Bounds <$> traverse (conv (bf^.alpha) "alpha") (bf^.alphas)
         <*> traverse (conv (bf^.beta) "beta")  (bf^.betas)
         <*> (traverse.traverse) (conv (bf^.h) "h") (bf^.hs)
         <*> (traverse.traverse) (conv (bf^.g) "g") (bf^.gs)
  where
    conv _ _ (Both x y) = pure (x,y)
    conv (Both x y) _ NoBound = pure (x,y)
    conv (Both x _) _ (JustUpper y) = pure (x,y)
    conv (Both _ y) _ (JustLower x) = pure (x,y)
    conv (JustLower x) _ (JustUpper y) = pure (x,y)
    conv (JustUpper x) _ (JustLower y) = pure (x,y)
    conv _ msg _ = unexpected $ "Unspecified bounds for " ++ msg

-- |
-- >>> :{
-- let sampleStr = unlines
--         [ "alpha has defaultUpperBound =  0."
--         , " "
--         , " alpha has defaultLowerBound =  20."
--         , " "
--         , " beta has defaultLowerBound =  0."
--         , " "
--         , " beta has defaultUpperBound =  20."
--         , " "
--         , " g has defaultLowerBound =  -4."
--         , " g_1_1 has lowerBound =  0."
--         , " g_2_2 has lowerBound =  0."
--         , " g_3_3 has lowerBound =  0."
--         , " "
--         , " g has defaultUpperBound =   4."
--         , " g_1_1 has upperBound =  0."
--         , " g_2_2 has upperBound =  0."
--         , " g_3_3 has upperBound =  0."
--         , " "
--         , " h has defaultLowerBound =  -4."
--         , " h_1_1 has lowerBound =  1.0E-15"
--         , " h_2_2 has lowerBound =  1.0E-15"
--         , " h_3_3 has lowerBound =  1.0E-15"
--         , " "
--         , " h has defaultUpperBound =   4." ]
-- in parseTest (getExprParser (bounds 4)) sampleStr
-- :}
-- Bounds {_boundsAlphas = [(20.0,0.0),(20.0,0.0),(20.0,0.0),(20.0,0.0)], _boundsBetas = [(0.0,20.0),(0.0,20.0),(0.0,20.0),(0.0,20.0)], _boundsHs = [[(1.0e-15,4.0),(-4.0,4.0),(-4.0,4.0),(-4.0,4.0)],[(-4.0,4.0),(1.0e-15,4.0),(-4.0,4.0),(-4.0,4.0)],[(-4.0,4.0),(-4.0,4.0),(1.0e-15,4.0),(-4.0,4.0)],[(-4.0,4.0),(-4.0,4.0),(-4.0,4.0),(-4.0,4.0)]], _boundsGs = [[(0.0,0.0),(-4.0,4.0),(-4.0,4.0),(-4.0,4.0)],[(-4.0,4.0),(0.0,0.0),(-4.0,4.0),(-4.0,4.0)],[(-4.0,4.0),(-4.0,4.0),(0.0,0.0),(-4.0,4.0)],[(-4.0,4.0),(-4.0,4.0),(-4.0,4.0),(-4.0,4.0)]]}
bounds :: Int -> ExperimentParser Bounds
bounds = getBounds <=< boundFill

data DataPoint = DataPoint
  { _dataPointTime ::  Double
  , _dataPointVars :: [Double]
  , _dataPointSdev :: [Double]
  } deriving Show

makeFields ''DataPoint

toDataPoint :: Sample -> DataPoint
toDataPoint (Sample _ _ t v s) = DataPoint t v s

updSample :: Int -> StateT (IntMap (IntMap DataPoint)) ExperimentParser ()
updSample n = do
  s <- lift (sample n)
  let exprI = s^.experimentNum
  let smplI = s^.num
  expr <- use (at exprI)
  expr' <- maybeToPT ("Unrecognised experiment: " ++ views experimentNum show s) expr
  case expr' ^. at smplI of
    Nothing -> at exprI . mapped . at smplI ?= toDataPoint s
    Just _ -> lift $ unexpected $ "Duplicate samples for experiment " ++ show exprI ++ ", sample " ++ show smplI

-- |
-- >>> :{
-- let sampleStr = unlines
--       [ "sample_22 of experiment_1"
--       , "has time =  0.42000000E+01"
--       , "has variable_ =   0.1908074706767553E+01  0.3267195980257653E+01  0.5606229424488210E+00  0.7500000000000000E+00"
--       , "has sdev of variable_ =  0.10000000E-01 0.10000000E-01 0.10000000E-01 0.00000000E+00"]
-- in parseTest (getExprParser (samples 1 4)) sampleStr
-- :}
-- fromList [(1,fromList [(22,DataPoint {_dataPointTime = 4.2, _dataPointVars = [1.908074706767553,3.267195980257653,0.560622942448821,0.75], _dataPointSdev = [1.0e-2,1.0e-2,1.0e-2,0.0]})])]
samples :: Int -> Int -> ExperimentParser (IntMap (IntMap DataPoint))
samples n i = execStateT (some $ updSample i) (IntMap.fromList [(j,mempty) | j <- [1..n]])

data VariableDataPoint = VariableDataPoint
  { _variableDataPointTime  :: Double
  , _variableDataPointValue :: Double
  , _variableDataPointSdev  :: Double }

toVarDataPoint :: DataPoint -> [VariableDataPoint]
toVarDataPoint (DataPoint dpt dpv dpd) = zipWith (VariableDataPoint dpt) dpv dpd

checkIntMap :: IntMap a -> Either Int [a]
checkIntMap = zipWithA f [1..] . IntMap.assocs where
  f i (n,x) | i == n = Right x
            | otherwise = Left i

data ExprEnvironment = ExprEnvironment
  { _exprEnvironmentExprNames :: [Text]
  , _exprEnvironmentVarNames  :: [Text] }

makeFields ''ExprEnvironment

toLists :: IntMap (IntMap DataPoint) -> ExperimentParser [[[VariableDataPoint]]]
toLists xs = do
  ys <- eitherToP (("Missing experiment " ++) . show) (checkIntMap xs)
  zs <- itraverse (\i -> eitherToP (\j -> concat ["Missing sample ", show j, " from experiment ", show i]) . checkIntMap) ys
  pure $ (map.map) toVarDataPoint zs

toExperiments :: ExprEnvironment -> [[[VariableDataPoint]]] -> ExperimentParser Experiment
toExperiments env = fmap (Experiment "expt1") . itraverse f where
  f i x =
    Network <$> maybeToP ("Unnammed experiment " ++ show i) (preview (exprNames . ix i) env)
            <*> itraverse ff (transpose x)
  ff i x = flip VariableData x <$>
    maybeToP ("Unnamed variable " ++ show i) (preview (varNames . ix i) env)

problem :: Parser (SSystem (Either (VarExpr Double) (Double,Double)), Experiment)
problem = getExprParser $ do
  whiteSpace
  vrs_ <- some varDecl
  let vrs = zipWith (set name) uniqNames vrs_
  let vlen = length vrs
  bds <- bounds vlen
  exprs <- some exprDecl
  let env =
        ExprEnvironment
          (map (Text.pack . view name) $ sortOn (view num) exprs)
          (map (Text.pack . view name) $ sortOn (view num) vrs)
  smpls <- samples (length exprs) vlen
  lsts <- toLists smpls
  expr <- toExperiments env lsts
  pure (toSSystem bds, expr)

data Experiment = Experiment
  { _experimentName     :: Text
  , _experimentNetworks :: [Network] }

data Network = Network
  { _networkName      :: Text
  , _networkVariables :: [VariableData] }

data VariableData = VariableData
  { _variableDataName :: Text
  , _variableDataVals :: [VariableDataPoint] }


-- | Converts an experiment to the required python file
-- format.
-- >>> :{
-- let exampleExp =
--       Experiment (Text.pack "expt1")
--         [ Network (Text.pack "net1")
--           [ VariableData (Text.pack "x")
--             [ VariableDataPoint 0 0 0.1
--             , VariableDataPoint 1 1 0.1
--             , VariableDataPoint 2 2 0.1 ]
--           , VariableData (Text.pack "y")
--             [ VariableDataPoint 0 0 0.1
--             , VariableDataPoint 1 1 0.1
--             , VariableDataPoint 2 2 0.1 ] ] ]
-- in (putStr . Text.unpack . toExpFormat) exampleExp
-- :}
-- from SloppyCell.ReactionNetworks import *
-- expt = Experiment('expt1')
-- data = {
--   'net1':{
--     'x': {
--       0.0: (0.0, 0.1),
--       1.0: (1.0, 0.1),
--       2.0: (2.0, 0.1),
--     },
--     'y': {
--       0.0: (0.0, 0.1),
--       1.0: (1.0, 0.1),
--       2.0: (2.0, 0.1),
--     },
--   },
-- }
-- expt.set_data(data)

toExpFormat :: Experiment -> Text
toExpFormat (Experiment n ns) = Text.unlines $
  [ "from SloppyCell.ReactionNetworks import *"
  , Text.concat ["expt = Experiment('", n, "')"]
  , "data = {" ] ++
  (ns >>= fn) ++
  ["}", "expt.set_data(data)"]
  where
    fn (Network nn nvs) =
      Text.concat ["  '", nn, "':{"] : (nvs >>= fvd) ++ ["  },"]
    fvd (VariableData vn vls) =
      Text.concat ["    '", vn, "': {"] : map fvdp vls ++ ["    },"]
    fvdp (VariableDataPoint t v sd) =
      Text.concat ["      ", repr t, ": (", repr v, ", ", repr sd, "),"]
    repr = Text.pack . show

makeFields ''Experiment
makeFields ''Network
makeFields ''VariableData
makeFields ''VariableDataPoint
