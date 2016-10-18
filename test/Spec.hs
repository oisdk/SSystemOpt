{-# LANGUAGE TemplateHaskell #-}

import           Test.DocTest
import           Test.QuickCheck


prop_one_is_one :: Property
prop_one_is_one = once $ 1 === (1 :: Int)

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = do
  doctest [ "-isrc"
          , "src/Text/Parse/ODEBench.hs"
          , "src/Numeric/Solver.hs"
          , "src/Numeric/Search.hs"
          , "src/Numeric/SSystem.hs" ]
  runTests
