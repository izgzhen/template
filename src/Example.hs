module Example where

import AST
import TypeCheck
import Compiler

{-
    $((\(s : Int -> TyQ) -> (s 1)) (\(i: Int) -> [| $i + $i |]))

    -- should be (1 + 1) after stage 1
    -- and evaluate to 2 
-}

tm1 :: Term
tm1 = TmSplice $
        TmApp
            (TmAbs "s" (TyArrow TyInt TyQ)
                (TmApp (TmVar "s") (TmInt 1)))
            (TmAbs "i" TyInt
                (TmBracket $ TmApp (TmApp (TmVar "plus") (TmSplice $ TmVar "i")) (TmSplice $ TmVar "i")))

                
{-
    $((\(s : String) -> TmAbs s TyInt (TyVar s)) gensym)

    -- should be (_x0)
-}

tm2 :: Term
tm2 = TmSplice $
        TmApp
            (TmAbs "s" TyString
                $ TmApp (TmApp (TmApp (TmVar "TmAbs") (TmVar "s")) (TmVar "TyInt"))
                        (TmApp
                            (TmVar "TmVar")
                            (TmVar "s")))
            (TmVar "genstr")
