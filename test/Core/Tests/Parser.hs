module Core.Tests.Parser (parserTests) where

import Test.Tasty
import Test.Tasty.HUnit

import Core.Language
import Core.Parser

testPrefix :: String
testPrefix = "Tests.Parser."

parserTests :: [TestTree]
parserTests = [ test_sample_I
              , test_sample_id
              , test_sample_id_twice
              , test_sample_updating
              , test_sample_arithm
              ] 

src_dir = "samples/"

testParseFile :: FilePath -> CoreProgram -> Assertion
testParseFile path expected = do
    actual <- parseCoreFile path
    assertEqual ("smoke test for " ++ path) expected actual

test_sample_I :: TestTree
test_sample_I = testCase (testPrefix ++ src)
    (testParseFile src    
        [ ("main", [], EAp (EVar "I") (ENum 3)) ] 
    ) where src = src_dir ++ "i.fc"
        
test_sample_id :: TestTree
test_sample_id = testCase (testPrefix ++ src)
    (testParseFile src
        [ ("id"  , [], EAp (EAp (EVar "S") (EVar "K")) (EVar "K"))
        , ("main", [], EAp (EVar "id") (ENum 3))
        ]
    ) where src = src_dir ++ "id.fc"
    
test_sample_id_twice :: TestTree
test_sample_id_twice = testCase (testPrefix ++ src)
    (testParseFile src
        [ ("id"  , [], EAp (EAp (EVar "S") (EVar "K")) (EVar "K"))
        , ("main", [], EAp (EAp (EAp (EAp (EVar "twice") (EVar "twice")) (EVar "twice")) (EVar "id")) (ENum 3))
        ] 
    ) where src = src_dir ++ "id_twice.fc"

test_sample_updating :: TestTree
test_sample_updating = testCase (testPrefix ++ src)
    (testParseFile src
        [ ("main", [], EAp (EAp (EVar "twice") (EAp (EAp (EVar "I") (EVar "I")) (EVar "I"))) (ENum 3)) ] 
    ) where src = src_dir ++ "updating.fc"

test_sample_arithm :: TestTree
test_sample_arithm = testCase (testPrefix ++ src)
    (testParseFile src
        [ ("main", [], EAp (EAp (EVar "twice") (EAp (EAp (EVar "I") (EVar "I")) (EVar "I"))) (ENum 3)) ] 
    ) where src = src_dir ++ "arithm.fc"




