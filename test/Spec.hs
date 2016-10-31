module Main where

import Control.DynamicScheduler
import Control.Monad
import Test.HUnit
import Data.Maybe
import TestUtils

main :: IO ()
main = void $ runTestTT unitTests

unitTests :: Test
unitTests = TestList
  [
    testMkSeconds
  , testMkExecutorCount
  ]

testMkSeconds :: Test
testMkSeconds = testSuite
  [
    assertEqual "mkSeconds: negative number is nothing"
                Nothing
                (mkSeconds (-1))

  , assertBool  "mkSeconds: 0  is Just"
                (isJust $ mkSeconds 0)

  , assertBool  "mkSeconds: >0 is Just"
                (isJust $ mkSeconds 100)
  ]

testMkExecutorCount :: Test
testMkExecutorCount = testSuite
  [
    assertEqual "mkExecutorCount: negative number is unlimited"
                Unlimited
                (mkExecutorCount (-1))

  , assertEqual "mkExecutorCount: 0  is Unlimited"
                Unlimited
                (mkExecutorCount 0)
  , assertEqual "mkExecutorCount: >0 is Limited"
                (Limited 10)
                (mkExecutorCount 10)
  ]
