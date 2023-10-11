module Core.Language where

{-
    Input Core program:
    ```
      main = double 21 ;
      double x = x+x
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
type Program a = [ScDefn a]
type CoreProgram = Program Name

-- A supercombinator (toplevel) definition: name, args, body
type ScDefn a = (Name, [a], Expr a)
type CoreScDefn = ScDefn Name

-- Expressions
data Expr a = EVar Name               -- Variables
            | ENum Int                -- Numbers
            | EConstr Int Int         -- Construtor with tag and arity
            | EAp (Expr a) (Expr a)   -- Application
            | ELet                    -- Let(rec) expressions
                IsRec                 --   Recursive flag
                [(a, Expr a)]         --   Definitions
                (Expr a)              --   Body
            | ECase                   -- Case expression
                (Expr a)              --   Expression to  match
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

isApp :: Expr t -> Bool
isApp (EAp _ _) = True
isApp _         = False

isCompound :: Expr t -> Bool
isCompound (ENum _)      = False
isCompound (EVar _)      = False
isCompound (EConstr _ _) = False
isCompound _             = True
