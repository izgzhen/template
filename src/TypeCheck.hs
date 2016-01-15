module TypeCheck (
  typeCheck
, initTyEnv
) where

import AST
import Data.Maybe (maybe)
import qualified Data.Map as M

type Env = M.Map Name Type

initTyEnv :: Env
initTyEnv = M.empty

-- Stage 1 type check

typeCheck :: Env -> Term -> Either String Type
typeCheck env (TmInt _) = return TyInt
typeCheck env (TmVar x) = maybe (Left $ "no such var: " ++ x) Right $ M.lookup x env
typeCheck env (TmApp tm1 tm2) = do
    ty1 <- typeCheck env tm1
    ty2 <- typeCheck env tm2
    case ty1 of
        TyArrow ty2 ty3 -> Right ty3
        _ -> Left $ "Illegal type for TmApp: " ++ show ty1 ++ " and " ++ show ty2

typeCheck env (TmAbs x ty tm) = do
    tytm <- typeCheck (M.insert x ty env) tm
    return $ TyArrow ty tytm

typeCheck env (TmSplice tm) = do
    tytm <- typeCheck env tm
    if tytm == TyQ then
        return TyBottom
        else error $ "TmSplice inner " ++ show tm ++ "is not typed as TyQ, but is " ++ show tytm

typeCheck env (TmBracket tm) = return TyQ
typeCheck env (TmType _) = return TyType
typeCheck env (TmTm _)   = return TyQ

