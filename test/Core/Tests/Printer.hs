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
    testExpressions 


-- Expressions

testExpressions :: [TestTree]
testExpressions = [test_I, test_K, test_K1, test_compose]

test_I :: TestTree
test_I = testCase (testPrefix ++ "test_I")
    (assertEqual "I builtin: x" "x" (pprintExpr $ getScDefnExpr builtinI))

test_K :: TestTree
test_K = testCase (testPrefix ++ "test_K")
    (assertEqual "K builtin: x" "x" (pprintExpr $ getScDefnExpr builtinK))

test_K1 :: TestTree
test_K1 = testCase (testPrefix ++ "test_K1")
    (assertEqual "K1 builtin: y" "y" (pprintExpr $ getScDefnExpr builtinK1))

test_compose :: TestTree
test_compose = testCase (testPrefix ++ "test_compose")
    (assertEqual "compose builtin: f g x" "f g x" (pprintExpr $ getScDefnExpr builtinCompose))

