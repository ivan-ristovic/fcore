module Core.Template 
    ( runCoreProgram
    , runCoreFile
    ) where

import Control.Arrow ((>>>))
import Control.Monad

import Core.Language
import Core.Parser
import Core.Prelude
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


extraPreludeDefs = []

compile :: CoreProgram -> TiState
compile p = TiState initStack initTiDump initHeap globals tiStatsInitial
    where initStack  = [mainAddr]
          mainAddr   = aLookup globals "main" (error "main is not defined")
          initTiDump = DummyTiDump 
          (initHeap, globals) = builtInitialHeap scDefs
          scDefs     = p ++ preludeDefs ++ extraPreludeDefs 

builtInitialHeap :: [CoreScDef] -> (TiHeap, TiGlobals)
builtInitialHeap scDefs = mapAccuml allocateSc hInitial scDefs
    where allocateSc heap (name, args, body) = (\addr -> (name, addr)) <$> hAlloc heap (NSc name args body)
    
eval :: TiState -> [TiState]
eval state = state:states
    where states
            | isFinalState state = []
            | otherwise          = eval next_state
          next_state = updateStats $ step state

updateStats :: TiState -> TiState
updateStats = applyToStats (\s -> s { tiStatsSteps = tiStatsSteps s + 1 })

isFinalState :: TiState -> Bool
isFinalState (TiState [] _ heap _ _)          = error "empty stack"     -- unreachable
isFinalState (TiState [sole_addr] _ heap _ _) = isDataNode (hLookup heap sole_addr)
isFinalState _                                = False

isDataNode :: Node -> Bool
isDataNode (NNb _) = True
isDataNode _       = False

step :: TiState -> TiState
step state@(TiState (addr:_) dump heap globals stats) = dispatch $ hLookup heap addr
    where dispatch (NNb n)            = stepNb state n
          dispatch (NAp a1 a2)        = stepAp state a1 a2
          dispatch (NSc sc args body) = stepSc state sc args body

stepNb :: TiState -> Int -> TiState
stepNb state n = error "error: Number applied as a function!"

stepAp :: TiState -> Addr -> Addr -> TiState
stepAp state a1 _ = state { tiStateStack = a1 : (tiStateStack state) }

stepSc :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSc = undefined 

showResults :: [TiState] -> String
showResults ss = show ss
