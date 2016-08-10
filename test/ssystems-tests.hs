{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import           Data.Functor
import qualified Data.Text.Lazy.IO    as T
import           Experiments
import           Parse
import           SBML
import           System.Exit
import           Test.DocTest
import           Test.QuickCheck      hiding (listOf)
import           Text.Taggy.DOM
import           Text.Trifecta.Parser

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

prop_parseList :: OrderedList Double -> Property
prop_parseList (Ordered xs) = case xs of
  (x1:x2:x3:_) -> check3 x1 x2 x3 .&. check2 x1 x2 .&. check1 x1 .&. checkNil
  (x1:x2:_) -> check2 x1 x2 .&. check1 x1 .&. checkNil
  (x1:_) -> check1 x1 .&. checkNil
  [] -> checkNil
  where
    check3 x y z = Right [x,y..z] === parseTester (listOf double') (showThree x y z)
    check2 x y = Right [x..y] === parseTester (listOf double') (showTwo x y)
    check1 x = Right [x] === parseTester (listOf double') (showOne x)
    showThree x y z = concat ["[", show x, ", ", show y, "..", show z, "]"]
    showTwo x y = "[" ++ show x ++ ".." ++ show y ++ "]"
    showOne x = "[" ++ show x ++ "]"
    checkNil = Right [] === parseTester (listOf double') "[]"

sbmlFile :: String
         -> String
         -> IO ()
sbmlFile inFile outFile = do
  ss <- parseSystemFromFile inFile >>= toFail
  xf <- T.readFile outFile
  sb <- (toFail . maybe (Left "No xml parse") Right . headMay . parseDOM False) xf
  single ("Code in " ++ inFile ++ " doesn't generate SBML in " ++ outFile) (toSBML ss == sb)

odeFile :: String
        -> String
        -> IO ()
odeFile inFile outFile = do
  (_,e) <- maybe exitFailure pure =<< parseFromFile problem inFile
  ef <- T.readFile outFile
  single ("Problem in " ++ inFile ++ " doesn't generate Python in" ++ outFile) (toExpFormat e == ef)

toFail :: Either String a -> IO a
toFail = either (\e -> single e False *> exitFailure) pure

single :: String -> Bool -> IO ()
single e = void . quickCheckExit . once . counterexample e . property

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r

return []

runTests :: IO Bool
runTests = $forAllProperties quickCheckExit

main :: IO Bool
main = do
  doctest
    [ "-isrc"
    , "src/Parse.hs"
    , "src/SBML.hs"
    , "src/Search.hs"
    , "src/Solver.hs"
    , "src/SSystem.hs"
    , "src/Utils.hs"
    , "src/Experiments.hs"
    , "app/Configure.hs"]
  sbmlFile "ExampleModels/Model1/model.txt" "ExampleModels/Model1/sbml.xml"
  odeFile "ExampleModels/ss_cascade/experiment.txt" "ExampleModels/ss_cascade/result.txt"
  runTests
