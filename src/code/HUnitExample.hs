module Main where

import Control.Exception
import Test.HUnit

divider x y = x `div` y

test_div_by_zero = TestCase $
  assertEqual "div by zero is Infinity" (divider 1 0) 42
  --assertFailure "div by zero is Infinity" (divider 1 0)

test_1_div_2_eq_0 = TestCase $
  assertEqual "fractions round down" (divider 1 2) 0

tests = TestList [ TestLabel "test1" test_div_by_zero,
                   TestLabel "test2" test_1_div_2_eq_0 ]

--main :: IO ()
main = do
  (try $ putStrLn "hello" :: IO (Either SomeException ()))
  runTestTT tests
