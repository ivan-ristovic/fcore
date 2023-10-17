module Core.Template 
    ( runCoreProgram
    , runCoreFile
    ) where

import Control.Arrow ((>>>))
import Control.Monad

import Core.Language
import Core.Parser
import Core.Utils

-- Template instantiation machine state
data TiState = TiState 
    
    -- Stack of addresses, each identifying a node.
    -- The stack forms the spine of the expr under evaluation.
    { tiStateStack   :: TiStack

    -- State of spine stack prior to the evaluation of a strict primitive.
    , tiStateDump    :: TiDump

    -- Collection of tagged nodes
    , tiStateHeap    :: TiHeap
    
    -- Node addresses of supercombinators and primitives.
    , tiStateGlobals :: TiGlobals

    -- Runtime stats
    , tiStateStats   :: TiStats
    
    } deriving (Show)

type TiStack = [Addr]
data TiDump = DummyTiDump deriving Show
type TiHeap = Heap Node

data Node = NAp Addr Addr
          | NSc Name [Name] CoreExpr
          | NNb Int
          deriving (Show, Eq)

type TiGlobals = [(Name, Addr)]

data TiStats = TiStats
    { tiStatsSteps :: Int
    } deriving (Show)

tiStatsInitial :: TiStats
tiStatsInitial = TiStats 0

tiStatsIncSteps :: TiStats -> TiStats
tiStatsIncSteps s = s { tiStatsSteps = (tiStatsSteps s) + 1 }

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = state { tiStateStats = f (tiStateStats state) }


runCoreProgram :: String -> Either ParseError String
runCoreProgram = parseCoreProgram >>> fmap (compile >>> eval >>> showResults)

runCoreFile :: FilePath -> IO ()
runCoreFile = parseCoreFile >=> (compile >>> eval >>> showResults >>> putStrLn) 


compile :: CoreProgram -> TiState
compile p = TiState [] DummyTiDump hInitial []

eval :: TiState -> [TiState]
eval s = [s]

showResults :: [TiState] -> String
showResults ss = "Hello"
