{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Functor
import           Data.Ord
import           Data.Serialize           (Serialize, decode, encode)
import           Expr
import           Parse
import           Square
import           SSystem
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

toList :: Foldable f => f a -> [a]
toList = foldr (:) []

prop_ParseExpr :: Expr -> P.Result
prop_ParseExpr = checkParse expr (`roundShow` "") (`prettyPrint` "") approxEqual

prop_correctSize :: Square Integer -> Bool
prop_correctSize s = n * n == length s where n = _squareSize s

prop_listIso :: NonEmptyList Integer -> Bool
prop_listIso (NonEmpty xs) = sameResult (Just . take m) (fmap toList . fromList n) xs where
  n = (floor . sqrt' . fromIntegral . length) xs
  m = n * n
  sqrt' :: Double -> Double
  sqrt' = sqrt

prop_listRev :: Square Integer -> Bool
prop_listRev s = sameResult Just (fromList n . toList) s where
  n = _squareSize s

prop_Indexing :: Square Integer -> Bool
prop_Indexing s = map (unsafeIndex s) ((,) <$> idxs <*> idxs) == toList s where
  idxs = [0..(_squareSize s - 1)]

prop_Ordering :: Square Integer -> Square Integer -> Property
prop_Ordering s t = classify (c==EQ) "Same size squares" . classify (c/=EQ) "Different sized squares" $
  case c of
    EQ -> r == comparing toList s t
    _  -> r == c
    where
      c = comparing _squareSize s t
      r = compare s t

prop_BinSquare :: Square Int -> P.Result
prop_BinSquare = checkSerialize

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
