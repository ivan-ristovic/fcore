module Core.Tests.Printer (printerTests) where

import Core.Language
import Core.Prelude
import Core.Printer
import Test.Tasty
import Test.Tasty.HUnit

testPrefix :: String
testPrefix = "Tests.Printer."


printerTests :: [TestTree]
printerTests = 
    testPrintBuiltins 


-- Expressions

testPrintBuiltins :: [TestTree]
testPrintBuiltins = [test_I, test_K, test_K1, test_S, test_compose]

test_I :: TestTree
test_I = testCase (testPrefix ++ "test_I")
    (assertEqual "I builtin" "I x = x" (pprint [builtinI]))

test_K :: TestTree
test_K = testCase (testPrefix ++ "test_K")
    (assertEqual "K builtin" "K x y = x" (pprint [builtinK]))

test_K1 :: TestTree
test_K1 = testCase (testPrefix ++ "test_K1")
    (assertEqual "K1 builtin" "K1 x y = y" (pprint [builtinK1]))

test_S :: TestTree
test_S = testCase (testPrefix ++ "test_S")
    (assertEqual "S builtin" "S f g x = f x (g x)" (pprint [builtinS]))

test_compose :: TestTree
test_compose = testCase (testPrefix ++ "test_compose")
    (assertEqual "compose builtin" "compose f g x = f (g x)" (pprint [builtinCompose]))

