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
type TiDump  = [TiStack]
type TiGlobals = [(Name, Addr)]

type TiHeap  = Heap Node
data Node = NAp Addr Addr               -- Application
          | NSc Name [Name] CoreExpr    -- Supercombinator
          | NNb Int                     -- Number
          | NIn Addr                    -- Indirection
          | NPr Name Primitive          -- Primitive
          deriving (Show, Eq)

data Primitive = Neg | Add | Sub | Mul | Div 
    deriving (Show, Eq)


-- Template instantiation machine stats tracking

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
          initTiDump = [] 
          (initHeap, globals) = buildInitialHeap scDefs
          scDefs     = p ++ preludeDefs ++ extraPreludeDefs 

buildInitialHeap :: [CoreScDef] -> (TiHeap, TiGlobals)
buildInitialHeap sc_defs = (heap'', sc_addrs ++ prim_addrs) 
    where (heap', sc_addrs) = mapAccuml allocateSc hInitial sc_defs
          allocateSc heap (name, args, body) = (\addr -> (name, addr)) <$> hAlloc heap (NSc name args body)
          (heap'', prim_addrs) = mapAccuml allocatePr heap' primitives
          allocatePr heap (name, prim) = let (h, a) = hAlloc heap (NPr name prim)
                                          in (h, (name, a))

primitives :: [(Name, Primitive)]
primitives = [ (opAneg, Neg)
             , (opAadd, Add), (opAsub, Sub)
             , (opAmul, Mul), (opAdiv, Div)
             ]
    
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
isFinalState (TiState [] _ _ _ _)              = panic "empty stack"
isFinalState (TiState [sole_addr] [] heap _ _) = isDataNode (hLookup heap sole_addr)
isFinalState _                                 = False

isDataNode :: Node -> Bool
isDataNode (NNb _) = True
isDataNode _       = False

step :: TiState -> TiState
step state@(TiState (addr:_) _ heap _ _) = dispatch $ hLookup heap addr
    where dispatch (NNb n)            = stepNb state n
          dispatch (NAp a1 a2)        = stepAp state a1 a2
          dispatch (NSc sc args body) = stepSc state sc args body
          dispatch (NIn a)            = stepIn state a
          dispatch (NPr name prim)    = stepPr state prim
step _ = panic "step: empty stack"

stepNb :: TiState -> Int -> TiState
stepNb (TiState [_] (stack:dump) heap globals stats) _ = TiState stack dump heap globals stats
stepNb (TiState _ (_:_) _ _ _) _ = err $ "stepNb: wrong stack detected" 
stepNb (TiState _ _ _ _ _)     _ = err $ "stepNb: wrong dump detected"

stepAp :: TiState -> Addr -> Addr -> TiState
stepAp (TiState stack@(topAddr:_) dump heap globals stats) a1 a2
  = case arg of
      NIn a3 -> TiState stack dump (makeHeap a3) globals stats
      _      -> TiState (a1:stack) dump heap globals stats
    where
        makeHeap = hUpdate heap topAddr . NAp a1
        arg      = hLookup heap a2
stepAp _ _ _ = err $ "stepAp: empty stack"

stepSc :: TiState -> Name -> [Name] -> CoreExpr -> TiState
stepSc (TiState stack dump heap globals stats) sc args body 
    | argc + 1 <= length stack = TiState stack'' dump heap'' globals stats
    | otherwise = err $ sc ++ " applied to too few arguments"
    where root_addr:stack' = drop argc stack
          stack'' = result_addr:stack'
          argc = length args
          (heap', result_addr) = instantiate body heap env
          heap'' = hUpdate heap' root_addr (NIn result_addr)
          env = arg_bindings ++ globals
          provided_args = getargs heap stack
          arg_bindings = zip args provided_args 

stepIn :: TiState -> Addr -> TiState
stepIn (TiState (_:stack) dump heap globals stats) addr = TiState (addr:stack) dump heap globals stats
stepIn _ _ = panic "indirection step: empty stack"

stepPr :: TiState -> Primitive -> TiState
stepPr state Neg = primNeg state
stepPr state Add = primArith state (+)
stepPr state Sub = primArith state (-)
stepPr state Mul = primArith state (*)
stepPr state Div = primArith state div

primNeg :: TiState -> TiState
primNeg (TiState stack@(_:_:_) dump heap globals stats)
    | isDataNode arg = TiState negApStack dump heap' globals stats
    | otherwise = TiState [argAddr] (negApStack:dump) heap globals stats
    where
        _:negApStack@(rootAddr:_) = stack
        heap' = hUpdate heap rootAddr (NNb $ negate value)
        argAddr:_ = getargs heap stack
        arg = hLookup heap argAddr
        NNb value = arg
primNeg _ = err "negate: not enough args)"

primArith :: TiState -> (Int -> Int -> Int) -> TiState
primArith (TiState stack@(_:_:_:_) dump heap globals stats) f
    | arg1IsDataNode && arg2IsDataNode = TiState ap2Stack dump heap' globals stats
    | arg2IsDataNode = TiState [arg1Addr] (ap1Stack:dump) heap globals stats
    | otherwise = TiState [arg2Addr] (ap2Stack:dump) heap globals stats
    where
        heap' = hUpdate heap rootAddr (NNb $ f v1 v2)
        _:ap1Stack = stack
        _:ap2Stack = ap1Stack
        rootAddr:_ = ap2Stack
        arg1Addr:arg2Addr:_ = getargs heap stack
        arg1 = hLookup heap arg1Addr
        arg2 = hLookup heap arg2Addr
        arg1IsDataNode = isDataNode arg1
        arg2IsDataNode = isDataNode arg2
        NNb v1 = arg1
        NNb v2 = arg2
primArith _ _ = err "primArith: wrong stack detected"

getargs :: TiHeap -> TiStack -> [Addr]
getargs heap (_:stack) = map getarg stack
    where getarg addr = case hLookup heap addr of (NAp _ arg) -> arg
                                                  _           -> panic "expected NAp"
getargs _ _ = panic "getargs: empty stack"


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
            -- , space, parens (showNode (hLookup heap fun_addr))
            , space, showFWAddr arg_addr
            , space, parens (showNode (hLookup heap arg_addr))
            ]
showStackNode _ node = showNode node

showNode :: Node -> Doc
showNode (NAp a1 a2) =
    mconcat [ text "NAp ", showAddrD a1
            , space      , showAddrD a2
            ]
showNode (NSc name _args _body) = text "NSc" <+> text name
showNode (NNb n) = text "NNb" <+> int n
showNode (NIn a) = text "NIn" <+> parens (showAddrD a)
showNode (NPr _ prim) = text "NPr" <+> text (show prim)

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

