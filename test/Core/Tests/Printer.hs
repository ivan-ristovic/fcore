module Core.Tests.Printer (printerTests) where

import Core.Language
import Core.Prelude
import Core.Printer
import Test.Tasty
import Test.Tasty.HUnit

testPrefix :: String
testPrefix = "Tests.Printer."


printerTests :: [TestTree]
printerTests = testPrintBuiltins 
            ++ testPrintLet


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


testPrintLet :: [TestTree]
testPrintLet = [test_letSimple, test_letComplex] -- test_caseof, test_lambda]

test_letSimple :: TestTree
test_letSimple = testCase (testPrefix ++ "test_letSimple")
    (assertEqual "simple let" "f = let twice = x * x\n     in twice x" (pprint simpleLet))
    where simpleLet = [("f", [], letExpr)] 
          letExpr   = ELet False defs body
          defs      = [("twice", EAp (EAp (EVar opAmul) (EVar "x")) (EVar "x"))]
          body      = EAp (EVar "twice") (EVar "x")

test_letComplex :: TestTree
test_letComplex = testCase (testPrefix ++ "test_letComplex")
    (assertEqual 
        "complex let" 
        "g = letr p = x * y;\n         s = x + y\n      in p - s" 
        (pprint simpleLet)
    )
    where simpleLet = [("g", [], letExpr)] 
          letExpr   = ELet True defs body
          defs      = [ ("p", EAp (EAp (EVar opAmul) (EVar "x")) (EVar "y"))
                      , ("s", EAp (EAp (EVar opAadd) (EVar "x")) (EVar "y")) 
                      ]
          body      = EAp (EAp (EVar opAsub) (EVar "p")) (EVar "s")

test_caseof :: TestTree
test_caseof = undefined

test_lambda :: TestTree
test_lambda = undefined

