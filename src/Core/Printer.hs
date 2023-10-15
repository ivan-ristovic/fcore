module Core.Printer (pprint) where

import Prelude hiding ((<>))

import Core.Language
import Text.PrettyPrint 


pprint :: CoreProgram -> String
pprint = render . fsep . programToDoc


-- Doc converters for Core constructs

programToDoc :: CoreProgram -> [Doc]
programToDoc = map scDefToDoc

scDefToDoc :: CoreScDef -> Doc
scDefToDoc (name, args, expr) = nameStr <+> varsStr <+> text opAssign <+> exprStr 
    where nameStr = text name
          varsStr = varListToDoc args
          exprStr = exprToDoc 1 expr

varListToDoc :: [Name] -> Doc
varListToDoc = hsep . map text

exprToDoc :: Int -> CoreExpr -> Doc
exprToDoc _ (ENum n) = int n
exprToDoc _ (EVar v) = text v
exprToDoc _ (ECons t ar) = text kwCons <+> braces (int t <> comma <> int ar)

exprToDoc prec (EAp (EAp (EVar b) e1) e2)
    | Just p <- lookup b binaryOps = parenIf (prec > p) $ exprToDoc p e1 <+> text b <+> exprToDoc p e2
    -- if b is not defined binary operator, skip

exprToDoc prec (EAp e1 e2)
    =  parenIf (isCompoundExpr e1 && not (isApExpr e1)) (exprToDoc prec       e1)
   <+> parenIf (isCompoundExpr e2)                      (exprToDoc (prec + 1) e2)

exprToDoc prec (ELet isRec defs expr) 
    = hang (text (kwLet isRec)) 3 (defsToDoc defs) 
   $$ text padLeftKwIn <+> exprToDoc prec expr
   where padLeftKwIn = (replicate times ' ') ++ kwIn
         times       = (length $ kwLet isRec) - (length kwIn)

exprToDoc prec (ECase expr alts) = hang caseof 2 (altsToDoc alts)
    where caseof = text kwCase
                <+> parenIf (isCompoundExpr expr) (exprToDoc prec expr)
                <+> text kwOf
          altsToDoc = vcat . map altToDoc
          altToDoc (tag, vars, e) = text opLBrace <> int tag <> text opRBrace
                                 <+> varListToDoc vars
                                 <+> text opRArrow <+> exprToDoc prec e <> semi

exprToDoc prec (ELam args body) 
    = text opLambda <> varListToDoc args <> text opDot <+> exprToDoc prec body

defsToDoc :: [(Name, CoreExpr)] -> Doc
defsToDoc= vcat . punctuate semi . map defToDoc

defToDoc :: (Name, CoreExpr) -> Doc
defToDoc (name, expr) = scDefToDoc (name, [], expr)


-- Helpers

parenIf :: Bool -> (Doc -> Doc)
parenIf True  = parens
parenIf False = id

binaryOps :: [(String, Int)]
binaryOps = 
    [(opAmul, 1), (opAdiv, 1), (opAadd, 2), (opAsub, 2)] ++
    map (\op -> (op, 3)) relationalOps ++
    [(opLand, 4), (opLor,5)]
