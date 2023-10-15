module Core.Parser 
    ( ParseError
    , parseCoreFile
    , parseCoreProgram
    ) where

import Control.Exception (Exception(), throw)
import Data.Functor
import Text.Parsec hiding (spaces)
import Text.Parsec.Language
import qualified Text.Parsec.Token as PT
import qualified Text.Parsec.Expr as PE
import Text.Parsec.String (Parser())

import Core.Language


instance Exception ParseError

parseCoreFile :: FilePath -> IO CoreProgram
parseCoreFile f = do
    contents <- readFile f
    case invokeParser f contents of 
        Left  err  -> throw err
        Right prog -> return prog

parseCoreProgram :: String -> Either ParseError CoreProgram
parseCoreProgram = invokeParser ""

invokeParser :: SourceName -> String -> Either ParseError CoreProgram
invokeParser = parse pProgram


coreStyle = haskellStyle { PT.reservedNames   = keywords
                         , PT.reservedOpNames = reservedOps
                         }
coreLexer  = PT.makeTokenParser coreStyle
parens     = PT.parens coreLexer
braces     = PT.braces coreLexer
identifier = PT.identifier coreLexer
reserved   = PT.reserved coreLexer
reservedOp = PT.reservedOp coreLexer
integer    = PT.integer coreLexer
int        = read <$> many1 digit
symbol     = PT.symbol coreLexer
operator   = PT.operator coreLexer
spaces     = PT.whiteSpace coreLexer
semi       = PT.semi coreLexer

binary op = PE.Infix e PE.AssocLeft 
    where e = do o <- symbol op
                 return (\x y -> EAp (EAp (EVar o) x) y)

sequence1 :: Parser a -> Parser [a]
sequence1 = PT.semiSep1 coreLexer

pProgram :: Parser CoreProgram
pProgram = spaces *> sequence1 pScDef

pScDef :: Parser CoreScDef
pScDef = do
    name <- identifier
    args <- many identifier
    reservedOp opAssign
    body <- pExpr
    return (name, args, body)

pExpr :: Parser CoreExpr
pExpr = choice [pLet, pCase, pLam, expr1] 
  where 
    expr1 = PE.buildExpressionParser table term
    table = [ map binary [opAmul, opAdiv]
            , map binary [opAadd, opAsub]
            , map binary relationalOps
            , [ binary opLand ]
            , [ binary opLor ]
            ]

    term = foldl1 EAp <$> many1 pAexpr

    pLam = do
        reservedOp opLambda
        params <- many1 identifier
        reservedOp opRArrow
        expr <- pExpr
        return (ELam params expr)

    pCase = do
        reserved kwCase
        e <- pExpr
        reserved kwOf
        alts <- sequence1 pAlt1
        return (ECase e alts)
        where
          pAlt1 = do
              i <- reservedOp opLBrace *> int <* reservedOp opRBrace
              vars <- many identifier
              reservedOp opRArrow
              expr <- pExpr
              return (i, vars, expr)

    pLet  = do
        isrec <- try (reserved kwLetNRec $> False) <|> (reserved kwLetRec $> True)
        binds <- sequence1 (try bind1)
        reserved kwIn
        expr <- pExpr
        return (ELet isrec binds expr)
        where
          bind1 = do
              name <- identifier
              reservedOp opAssign
              expr <- pExpr
              return (name, expr)

    pCtor = do
        reserved kwCons
        braces $ ECons <$> int <* reservedOp opComma <*> int

    pAexpr = spaces *> choice [pVar, pNum, pCtor, parens pExpr] <* spaces

    pNum  = ENum <$> integer
    pVar  = EVar <$> identifier
