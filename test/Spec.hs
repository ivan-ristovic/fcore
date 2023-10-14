import Core.Tests.Parser
import Core.Tests.Printer
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = 
    defaultMain 
        ( testGroup "Core Language Tests" 
          (  printerTests
          ++ parserTests
          )
        )


