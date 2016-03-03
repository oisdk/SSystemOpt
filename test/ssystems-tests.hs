{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main where

import           Control.Arrow
import           Control.Monad
import           Data.Binary              (decode, encode)
import           Data.Functor
import           Data.Text                (Text)
import           Expr
import           Parse
import           Prelude                  hiding (atan, cos, cosh, exp, log,
                                           sin, sinh, tan, tanh, (^))
import           SSystem
import           System.Exit
import           Test.QuickCheck
import qualified Test.QuickCheck.Property as P

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

instance Arbitrary a => Arbitrary (PTerm a) where
  arbitrary = oneof [ C <$> arbitrary, (:^:) <$> arbitrary <*> arbitrary ]

instance Arbitrary InitialDeclaration where arbitrary = ID <$> arbitrary <*> arbitrary

instance Arbitrary d => Arbitrary (PowerLawForm d) where
  arbitrary = PLawF <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary d => Arbitrary (SSystem d) where
  arbitrary = do
    variables <- arbitrary :: Gen [String]
    let inits = forM variables ((<$> arbitrary) . ID)
    let derivs = forM variables ((<*> arbitrary) . (<$> arbitrary) . PLawF)
    SSystem <$> inits <*> derivs

instance Arbitrary Parameter where
  arbitrary = Param <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (Configurable a) where
  arbitrary = Configurable <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary

prop_binaryIdExpr :: Expr -> Bool
prop_binaryIdExpr = isId (decode . encode)

prop_binaryIdConfig :: Configurable (Either Double Parameter) -> Bool
prop_binaryIdConfig = isId (decode . encode)

prop_basicDecl :: Property
prop_basicDecl = eachOf exampleInits (uncurry (parseTestProp initialAssign))

exampleInits :: [(InitialDeclaration, Text)]
exampleInits = [ (ID "x1" 1, "x1 = 1")
               , (ID "x3" (sin 1 + 4), "x3 = sin 1 + 4")
               , (ID "x4" (exp 1 ^ 2), "x4 = exp 1 ^ 2")
               , (ID "x5" (cos 1 + 4), "x5 = cos 1 + 4")
               , (ID "x6" 1, "x6 = 1")
               , (ID "x7" 1, "x7 = 1") ]

prop_basicDerivs :: Property
prop_basicDerivs = eachOf
  (first emptyName <$> exampleDerivs)
  ((uncurry . parseTestProp . fmap emptyName) diffForm) where
  emptyName = (fmap . fmap) ($"")

prop_wholeSystem :: Property
prop_wholeSystem = once $ either (const False) (const True) (parseSystem "" systemstr) where
  systemstr = "ddt x1 = x6 * x7 - x3;\n\
              \x1 = 1;\n\
              \x3 = sin 1 + 4;\n\
              \x4 = exp 1 ^ 2;\n\
              \x5 = cos 1 + 4;\n\
              \x6 = 1;\n\
              \x7 = 1;\n\
              \ddt x3 = x5 - 4;\n\
              \ddt x4 = 2 * x1 * x4 * x6 * x7 - 2 * x1 * x3 * x4;\n\
              \ddt x5 = 4 - x3;\n\
              \ddt x6 = x4 * x7 ^ -1 - x1 * x7 ^ -1;\n\
              \ddt x7 = x1 * x7;\n\
              \start = 0;\n\
              \stop = 5;\n\
              \absTolerance = 0.001;\n\
              \relTolerance = 0.001;\n\
              \steps = 10"

exampleDerivs :: [(PowerLawForm (Either Double (String -> Parameter)), Text)]
exampleDerivs = [ (PLawF x1 [c x6, c x7] [c x3]
                  , "ddt x1 = x6 * x7 - x3")
                , (PLawF x3 [c x5] [C 4]
                  , "ddt x3 = x5 - 4")
                , (PLawF x4 [C 2, c x1, c x4, c x6, c x7] [C 2, c x1, c x3, c x4]
                  , "ddt x4 = 2 * x1 * x4 * x6 * x7 - 2 * x1 * x3 * x4")
                , (PLawF x5 [C 4] [c x3]
                  , "ddt x5 = 4 - x3")
                , (PLawF x6 [c x4, x7 :^: Left (-1)] [c x1, x7 :^: Left (-1)]
                  , "ddt x6 = x4 * x7 ^ -1 - x1 * x7 ^ -1")
                , (PLawF x7 [c x1, c x7] []
                  , "ddt x7 = x1 * x7")
                ] where
  c v = v :^: Left 1
  (x1,x3,x4,x5,x6,x7) = ("x1","x3","x4","x5","x6","x7")

eachOf :: [a] -> (a -> P.Result) -> Property
eachOf l t = once $ foldr f P.succeeded l where
  f e a = maybe a (\p -> if p then a else r) (P.ok r) where r = t e

isId :: Eq a => (a -> a) -> a -> Bool
isId f x = f x == x
-- Evil version:
-- isId = (<*>) (==)

parseTestProp :: (Eq a, Show a) => ModelParser a -> a -> Text -> P.Result
parseTestProp p e s = maybe P.succeeded (\r -> P.failed { P.reason = r } ) (parseTester p e s)

quickCheckExit :: Testable prop => prop -> IO Result
quickCheckExit = resultExit <=< quickCheckResult where
  resultExit r@ Success{}  = pure r
  resultExit r = exitFailure $> r


return []
runTests = $forAllProperties quickCheckExit

main = runTests
