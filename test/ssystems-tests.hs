{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Ord
import           Data.Serialize           (Serialize, decode, encode)
import           Expr
import           Parse
import           SSystem
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

prop_ParseExpr :: Expr -> P.Result
prop_ParseExpr = checkParse expr roundShow prettyPrint approxEqual

prop_BinSSystem :: SSystem Int -> P.Result
prop_BinSSystem = checkSerialize

prop_BinExpr :: Expr -> P.Result
prop_BinExpr = checkSerialize

checkParse :: Parser a -> (a -> String) -> (a -> String) -> (a -> a -> Bool) -> a -> P.Result
checkParse p d s e x = either fw eq (parseTester p (s x)) where
  eq y | e x y = P.succeeded
       | otherwise = fw (concat ["Got     : ", d y, "\n", "Expected: ", d x])
  fw m = failWith $ m ++ "\nWith    : " ++ s x

sameResult :: Eq a => (b -> a) -> (b -> a) -> b -> Bool
sameResult = liftA2 (==)

sameResult2 :: Eq a => (c -> b -> a) -> (c -> b -> a) -> c -> b -> Bool
sameResult2 = liftA2 sameResult

isId :: Eq a => (a -> a) -> a -> Bool
isId = sameResult id

checkSerialize :: (Eq a, Serialize a) => a -> P.Result
checkSerialize a = either failWith (\x -> if x == a then P.succeeded else P.failed) . decode . encode $ a

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

failWith :: String -> P.Result
failWith r = P.failed { P.reason = r }

return []
runTests = $forAllProperties quickCheckExit

main = runTests
