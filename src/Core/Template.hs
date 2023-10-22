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

import Text.PrettyPrint hiding ((<>))


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


-- Template machine stats tracking

data TiStats = TiStats
    { tiStatsSteps         :: Int
    , tiStatsMaxStackDepth :: Int
    } deriving (Show)

tiStatsInitial :: TiStats
tiStatsInitial = TiStats 0 0

tiStatsIncSteps :: TiStats -> TiStats
tiStatsIncSteps s = s { tiStatsSteps = tiStatsSteps s + 1 }

tiStatsUpdMaxStackDepth :: Int -> TiStats -> TiStats
tiStatsUpdMaxStackDepth d s = s { tiStatsMaxStackDepth = max d (tiStatsMaxStackDepth s) }

applyToStats :: (TiStats -> TiStats) -> TiState -> TiState
applyToStats f state = state { tiStateStats = f (tiStateStats state) }


-- Entry points

type Verbosity = Int

runCoreProgram :: Verbosity -> String -> Either ParseError String
runCoreProgram v = parseCoreProgram >>> fmap (compile >>> eval >>> showResults v)

runCoreFile :: Verbosity -> FilePath -> IO ()
runCoreFile v = parseCoreFile >=> (compile >>> eval >>> showResults v >>> putStrLn) 


-- Compilation and Evaluation

extraPreludeDefs = []

compile :: CoreProgram -> TiState
compile p = TiState initStack initTiDump initHeap globals tiStatsInitial
    where initStack  = [mainAddr]
          mainAddr   = aLookup globals "main" (err "main is not defined")
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
updateStats state = state''
    where state'  = applyToStats (tiStatsUpdMaxStackDepth stackD) state
          state'' = applyToStats tiStatsIncSteps state'
          stackD  = length $ tiStateStack state

isFinalState :: TiState -> Bool
isFinalState (TiState [] _ _ _ _)             = panic "empty stack"
isFinalState (TiState [sole_addr] _ heap _ _) = isDataNode (hLookup heap sole_addr)
isFinalState _                                = False

isDataNode :: Node -> Bool
isDataNode (NNb _) = True
isDataNode _       = False

step :: TiState -> TiState
step state@(TiState (addr:_) _ heap _ _) = dispatch $ hLookup heap addr
    where dispatch (NNb n)            = stepNb state n
          dispatch (NAp a1 a2)        = stepAp state a1 a2
          dispatch (NSc sc args body) = stepSc state sc args body
step _ = panic "empty stack"

stepNb :: TiState -> Int -> TiState
stepNb n _ = err $ "number " ++ show n ++ " applied as a function"

stepAp :: TiState -> Addr -> Addr -> TiState
stepAp state a1 _ = state { tiStateStack = a1 : (tiStateStack state) }

stepSc :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSc (TiState stack dump heap globals stats) sc args body = TiState stack' dump heap' globals stats
    where stack' = result_addr : (drop (argc + 1) stack)
          argc = length args
          (heap', result_addr) = instantiate body heap env
          env = arg_bindings ++ globals
          provided_args = getargs heap stack
          arg_bindings
              | argc > length provided_args = err $ sc ++ " applied to too few arguments"
              | otherwise = zip args provided_args 

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (_:stack) = map getarg stack
    where getarg addr = case hLookup heap addr of (NAp _ arg) -> arg
                                                  _           -> panic "expected NAp"
getargs _ _ = panic "empty stack"


-- Instantiation

instantiate :: CoreExpr         -- sc body
            -> TiHeap           -- heap before
            -> [(Name, Addr)]   -- name to addr
            -> (TiHeap, Addr)   -- heap after and adress of instance root

instantiate (ENum n)    heap _   = hAlloc heap (NNb n)

instantiate (EAp e1 e2) heap env = hAlloc heap'' (NAp a1 a2)
    where (heap' , a1) = instantiate e1 heap  env
          (heap'', a2) = instantiate e2 heap' env

instantiate (EVar v) heap env = (heap, addr)
    where addr = aLookup env v (err ("undefined name " ++ show v))

instantiate (ELet isRec defs body) heap env = instantiate body heap' env'
    where (heap', env') = foldl accum (heap, env) defs
          accum (accHeap, accEnv) (var, expr) = 
               let (accHeap', addr) = instantiate expr accHeap chosenEnv
                   chosenEnv = if isRec then env' else env
                   accEnv' = (var, addr) : accEnv
               in (accHeap', accEnv')

instantiate (ECons _ _) _ _ = notImplemented "Cons instantiation" 

instantiate (ECase _ _) _ _ = notImplemented "caseof instantiation"

instantiate (ELam _ _) _ _ = notImplemented "lambda instantiation"


-- Pretty print results

showResults :: Verbosity -> [TiState] -> String
showResults v states = case v of 
    0 -> render fresult
    1 -> rend stats
    _ -> rend results
    where rend o  = render . vcat . punctuate (text "\n") $ [fresult] <> o
          results = map showState states <> stats
          stats   = [ showStats (fstate) ]
          fstate  = last states
          fresult = showFinalResult fstate 

showFinalResult :: TiState -> Doc
showFinalResult (TiState (addr:_) _ heap _ _) = showStackNode heap (hLookup heap addr)
showFinalResult _ = text "no result"

showState :: TiState -> Doc
showState (TiState stack _ heap _ _) = showStack heap stack $+$
                                       showHeap  heap

showHeap :: TiHeap -> Doc
showHeap = (text "H" <+>) . brackets
         . hcat . punctuate (comma <> space)
         . map showAddrD . hAddrs

showStack :: TiHeap -> TiStack -> Doc
showStack heap stack = text "S" <+> brackets (nest 2 (vcat items))
    where items = map showStackItem stack
          showStackItem addr = mconcat [ showFWAddr addr, text ": "
                                       , showStackNode heap (hLookup heap addr)
                                       ]

showStackNode :: TiHeap -> Node -> Doc
showStackNode heap (NAp fun_addr arg_addr) =
    mconcat [ text "NAp"
            , space, showFWAddr fun_addr
            , space, showFWAddr arg_addr
            , space, parens (showNode (hLookup heap arg_addr))
            ]
showStackNode _heap node = showNode node

showNode :: Node -> Doc
showNode (NAp a1 a2) =
    mconcat [ text "NAp ", showAddrD a1
            , space      , showAddrD a2
            ]
showNode (NSc name _args _body) = text "NSc" <+> text name
showNode (NNb n) = text "NNb" <+> int n

showAddrD :: Addr -> Doc
showAddrD addr = text $ aShow addr

showFWAddr :: Addr -> Doc
showFWAddr addr = pad <> text aStr
    where aStr = aShow addr
          pad  = mconcat (replicate (4 - length aStr) space)

showStats :: TiState -> Doc
showStats (TiState _ _ _ _ stats) =
    mconcat [ text "----- Stats -----\n"
            , text "Total steps     : ", int $ tiStatsSteps         $ stats
            , text "\n"
            , text "Max stack depth : ", int $ tiStatsMaxStackDepth $ stats
            , text "\n"
            ]

