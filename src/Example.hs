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

                
