{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Serialize           (Serialize, decode, encode)
import           SSystem
import Parse
import Data.List (sort)
import           System.Exit
import           Test.DocTest
import           Test.QuickCheck hiding (listOf)
import qualified Test.QuickCheck.Property as P

prop_BinSSystem :: SSystem Int -> P.Result
prop_BinSSystem = checkSerialize

singleList :: NonEmptyList Double -> Bool
singleList (NonEmpty x) = Right x == parseTester (listOf double') (show x)

doubleList :: Double -> Double -> Bool
doubleList x y = Right [a..b] == parseTester (listOf double') (list2 a b) where
  [a,b] = sort [x,y]
  list2 x y = "[" ++ show x ++ ".." ++ show y ++ "]"

tripleList :: Double -> Double -> Double -> Bool
tripleList x y z =  Right [a,b..c] == parseTester (listOf double') (list3 a b c) where
  [a,b,c] = sort [x,y,z]
  list3 x y z = concat ["[", show x, ",", show y, "..", show z, "]"]

-- checkParse :: Parser a -> (a -> String) -> (a -> String) -> (a -> a -> Bool) -> a -> P.Result
-- checkParse p d s e x = either fw eq (parseTester p (s x)) where
--   eq y | e x y = P.succeeded
--        | otherwise = fw (concat ["Got     : ", d y, "\n", "Expected: ", d x])
--   fw m = failWith $ m ++ "\nWith    : " ++ s x

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

main = do
  doctest
    [ "-isrc"
    , "src/Parse.hs"
    , "src/SBML.hs"
    , "src/Search.hs"
    , "src/Solver.hs"
    , "src/SSystem.hs"
    , "src/Utils.hs"
    , "app/Configure.hs"]
  runTests
