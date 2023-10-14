module Core.Printer (pprint, pprintExpr) where

import Core.Language

pprint :: CoreProgram -> String
pprint _ = "print()"

pprintExpr :: CoreExpr -> String
pprintExpr (ENum n)    = show n
pprintExpr (EVar v)    = v
pprintExpr (EAp e1 e2) = pprintExpr e1 ++ " " ++ pprintExpr e2
