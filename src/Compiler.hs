module Compiler (
  compile
, runCompile
) where

import Prelude hiding (lookup)
import qualified Data.Map as M
import AST
import Control.Monad.State

data Env = Env {
    _vals    :: M.Map Name (Compiler Term),
    _counter :: Int
}

initCompileEnv :: Env
initCompileEnv = Env {
    _vals = M.fromList [
                -- ("gensym", gensym),
                -- ("TmAbs", tmAbs),
                -- ("_TmAbs", \n t b -> T)
            ],
    _counter = 0
}

-- tmAbs :: Compiler Term
-- tmAbs = return $ TmAbs "name" TyString
--                     $ TmAbs "type" TyType
--                         $ TmAbs "body" TyTmTerm
--                             $ TmApp (TmApp (TmApp (TmVar ("_TmAbs")) (TmVar "name")) TmVar "type") TmVar "body"

-- gensym :: Compiler Term
-- gensym = do
--     i <- _counter <$> get
--     let ret = TmString $ "_x" ++ show i
--     modify (\s -> s { _counter = i + 1 })
--     return ret

type Compiler = State Env

vals :: Compiler (M.Map Name (Compiler Term))
vals = _vals <$> get

withVal :: Name -> Term -> Compiler a -> Compiler a
withVal x tm m = do
    oldVals <- vals
    modify (\s -> s { _vals = M.insert x (return tm) oldVals })
    ret <- m
    modify (\s -> s { _vals = oldVals })
    return ret

lookup :: Name -> Compiler Term
lookup x = do
    mTm <- M.lookup x <$> vals
    case mTm of
        Nothing -> error $ "no such var: " ++ x
        Just tm -> tm

-- Stage One

runCompile :: Compiler Term -> Term
runCompile = flip evalState initCompileEnv

compile :: Term -> Compiler Term
compile (TmApp tm1 tm2)  = TmApp <$> compile tm1 <*> compile tm2
compile (TmAbs x tyx tm) = TmAbs x tyx <$> compile tm
compile (TmSplice tm)    = evaluate tm
compile tm               = return tm

evaluate :: Term -> Compiler Term
evaluate (TmApp tmf tmx) = do
    tmf' <- evaluate tmf
    case tmf' of
        TmVar fx -> lookup fx
        TmAbs x _ tm -> withVal x tmx $ evaluate tm
        other -> error $ show other ++ " is not applicable"

evaluate (TmBracket tm)     = TmBracket <$> compile tm
evaluate (TmSplice tm)      = evaluate tm
evaluate (TmVar x)          = lookup x
evaluate tm@(TmInt _)       = return tm
evaluate tm@(TmString _)    = return tm
evaluate tm@(TmAbs _ _ _)   = return tm


