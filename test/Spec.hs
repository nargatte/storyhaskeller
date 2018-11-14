module Spec where

import Test.HUnit

import CodeParserTests

tests :: Test
tests = TestList [TestLabel "CodeParser" codeParserTests]

main :: IO ()
main = runTestTT tests
