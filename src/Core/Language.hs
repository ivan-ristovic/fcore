module Core.Language where

{-
    Input Core program:
    ```
      main = double 21 ;
      double x = x + x
    ```
    Representation:
    ```
      [
        ("main",   [],    (EAp (EVar "double") (ENum 21))),
        ("double", ["x"], (EAp (EAp (EVar "+") (EVar "x")) (EVar "x")))
      ]
    ```
-}

type Name = String

-- A core program is a list of supercombinator definitions
type Program a = [ScDef a]
type CoreProgram = Program Name

-- A supercombinator (toplevel) definition: name, args, body
type ScDef a = (Name, [a], Expr a)
type CoreScDef = ScDef Name

-- Expressions
data Expr a = EVar Name               -- Variables
            | ENum Int                -- Numbers
            | ECons Int Int           -- Construtor with tag and arity
            | EAp (Expr a) (Expr a)   -- Application
            | ELet                    -- Let(rec) expressions
                IsRec                 --   Recursive flag
                [(a, Expr a)]         --   Definitions
                (Expr a)              --   Body
            | ECase                   -- Case expression
                (Expr a)              --   Expression to match
                [Alter a]             --   Alternatives
            | ELam [a] (Expr a)       -- Lambda abstractions
            deriving (Show, Eq)
type CoreExpr = Expr Name

-- Alternative in case expression
type Alter a = (Int, [a], Expr a)
type CoreAlt = Alter Name

type IsRec = Bool

recursive :: IsRec
recursive = True

nonRecursive :: IsRec
nonRecursive = False

bindersOf :: [(a,b)] -> [a]
bindersOf = map fst

rhssOf :: [(a,b)] -> [b]
rhssOf = map snd

isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar _) = True
isAtomicExpr (ENum _) = True
isAtomicExpr _        = False

isApExpr :: Expr t -> Bool
isApExpr (EAp _ _) = True
isApExpr _         = False

isCompoundExpr :: Expr t -> Bool
isCompoundExpr (ENum _)      = False
isCompoundExpr (EVar _)      = False
isCompoundExpr (ECons _ _)   = False
isCompoundExpr _             = True

getScDefExpr :: CoreScDef -> CoreExpr
getScDefExpr (_, _, e) = e


-- Tokens

keywords = [kwLetNRec, kwLetRec, kwCase, kwIn, kwOf, kwCons]
kwLetNRec = "let"
kwLetRec  = "letr"
kwCase    = "case"
kwIn      = "in"
kwOf      = "of"
kwCons    = "Cons"
kwIf      = "if"  -- not counted as a keyword, since it is implemented as a primitive!

kwLet :: Bool -> String
kwLet True  = kwLetRec
kwLet False = kwLetNRec

reservedOps = [opAssign, opComma, opSColon, opLArrow, opRArrow, opLambda, opDot, opLBrace, opRBrace]
opAssign  = "="
opComma   = ","
opSColon  = ";"
opLArrow  = "<-"
opRArrow  = "->"
opLambda  = "\\"
opDot     = "."
opLBrace  = "{"
opRBrace  = "}"

relationalOps = [opReq, opRne, opRlt, opRle, opRgt, opRge]
opReq = "=="
opRne = "~="
opRlt = "<"
opRle = "<="
opRgt = ">"
opRge = ">="

arithmeticOps = [opAadd, opAsub, opAmul, opAdiv]
opAadd = "+"
opAsub = "-"
opAmul = "*"
opAdiv = "/"
opAneg = "negate" -- not counted in arithm ops table!

logicOps = [opLand, opLor]
opLand = "&"
opLor  = "|"

