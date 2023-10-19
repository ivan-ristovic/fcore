module Core.Prelude where

import Core.Language

preludeDefs :: [CoreScDef]
preludeDefs
  = [ builtinI
    , builtinK
    , builtinK1
    , builtinS
    , builtinCompose 
    , builtinTwice
    ]

builtinI :: CoreScDef
builtinI = ( "I" 
           , ["x"] 
           , EVar "x"
           ) 

builtinK :: CoreScDef
builtinK = ( "K"
           , ["x","y"]
           , EVar "x"
           ) 

builtinK1 :: CoreScDef
builtinK1 = ( "K1"
            , ["x","y"]
            , EVar "y"
            )

builtinS :: CoreScDef
builtinS = ( "S"
           , ["f","g","x"]
           , EAp (EAp (EVar "f") (EVar "x")) (EAp (EVar "g") (EVar "x"))
           )

builtinCompose :: CoreScDef
builtinCompose = ( "compose"
                 , ["f","g","x"]
                 , EAp (EVar "f") (EAp (EVar "g") (EVar "x"))
                 )

builtinTwice :: CoreScDef
builtinTwice = ( "twice"
             , ["f"]
             , EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")
             )
