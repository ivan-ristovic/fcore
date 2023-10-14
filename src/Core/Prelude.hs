module Core.Prelude where

import Core.Language

preludeDefs :: CoreProgram
preludeDefs
  = [ builtinI
    , builtinK
    , builtinK1
    , builtinS
    , builtinCompose 
    , builtinTwice
    ]

builtinI :: CoreScDefn
builtinI = ( "I" 
           , ["x"] 
           , EVar "x"
           ) 

builtinK :: CoreScDefn
builtinK = ( "K"
           , ["x","y"]
           , EVar "x"
           ) 

builtinK1 :: CoreScDefn
builtinK1 = ( "K1"
            , ["x","y"]
            , EVar "y"
            )

builtinS :: CoreScDefn
builtinS = ( "S"
           , ["f","g","x"]
           , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
           )

builtinCompose :: CoreScDefn
builtinCompose = ( "compose"
                 , ["f","g","x"]
                 , EAp (EVar "f") (EAp (EVar "g") (EVar "x"))
                 )

builtinTwice :: CoreScDefn
builtinTwice = ( "twice"
             , ["f"]
             , EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")
             )
