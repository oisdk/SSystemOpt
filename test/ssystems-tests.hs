{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Arrow
import           Control.Comonad
import           Control.Monad
import           Data.Serialize           (Serialize, decode, encode)
import           Data.Text                (Text)
import           Expr
import           Parse
import           Prelude                  hiding (atan, cos, cosh, exp, log,
                                           sin, sinh, tan, tanh, (^))
import           SSystem
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P
import           Utils
import Control.Applicative

instance Arbitrary Func where arbitrary = arbitraryBoundedEnum

instance Arbitrary Expr where
  arbitrary = sized (fmap Expr . anaM alg) where
    alg n | n <= 0 = CstF <$> arbitrary
          | otherwise = oneof [ flip FncF nest <$> arbitrary
                              , CstF <$> suchThat arbitrary (1000>)
                              , pure (NegF nest)
                              , pure (PowF nest nest)
                              , pure (PrdF nest nest)
                              , pure (SumF nest nest)
                              ] where nest = div n 8
prop_ParseODE :: P.Result
prop_ParseODE = parseTestProp (ODE
                               "x1"
                               (pStatic 0)
                               []
                               (pStatic 10)
                               [("x1", NumLearn (Right [0,0.5,1]))]
                              )  "ddt x1 = - 10 * x1 ^ [0,0.5 .. 1]"

prop_ParseLst1 :: P.Result
prop_ParseLst1 = parseTestProp [1.0 :: Double] "[1]"
prop_ParseLst2 :: P.Result
prop_ParseLst2 = parseTestProp ([] :: [Double]) "[]"
prop_ParseLst3 :: P.Result
prop_ParseLst3 = parseTestProp ([1,3..9] :: [Double]) "[1,3..9]"
prop_ParseLst4 :: P.Result
prop_ParseLst4 = parseTestProp ([1,3..9] :: [Double]) "[1,3 ..9]"

pStatic = NumLearn . Left

eachOf :: [a] -> (a -> P.Result) -> Property
eachOf l t = once $ foldr f P.succeeded l where
  f e a = maybe a (bool a r) (P.ok r) where r = t e

isId :: Eq a => (a -> a) -> a -> Bool
isId f x = x == f x
-- Evil version:
-- isId = (<*>) (==)

encodeDecode :: (Serialize a, Eq a) => a -> P.Result
encodeDecode e = either failWith (bool P.succeeded P.failed . (e==)) ((decode . encode) e)

sameResult :: Eq a => (b -> a) -> (b -> a) -> b -> Bool
sameResult = liftA2 (==)

parseTestProp :: (Eq a, Show a, Parse a) => a -> Text -> P.Result
parseTestProp e s = maybe P.succeeded failWith (parseTester parser e s)

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
