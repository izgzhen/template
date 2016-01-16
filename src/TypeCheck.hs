module TypeCheck (
  typeCheck
, initTyEnv
) where

import AST
import Data.Maybe (maybe)
import qualified Data.Map as M

type Env = M.Map Name Type

initTyEnv :: Env
initTyEnv = stdlib

-- Stage 1 type check

typeCheck :: Env -> Term -> Either String Type
typeCheck env (TmInt _) = return TyInt
typeCheck env (TmVar x) = maybe (Left $ "no such var: " ++ x) Right $ M.lookup x env
typeCheck env (TmApp tm1 tm2) = do
    ty1 <- typeCheck env tm1
    ty2 <- typeCheck env tm2
    case canonicalize ty1 of
        (ty2:ty:tys) -> Right $ compose $ ty : tys
        _ -> Left $ "Illegal type for TmApp: " ++ show tm1 ++ " : " ++ show ty1 ++
                    " and " ++ show tm2 ++ " : " ++ show ty2

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


stdlib = M.fromList [
              ("genstr", TyString)
            , ("TmAbs", TyArrow (TyArrow (TyArrow TyString TyType) TyQ) TyQ)
            , ("TmVar", TyArrow TyString TyQ)
            , ("TyInt", TyType)
            ]

canonicalize :: Type -> [Type]
canonicalize (TyArrow ty1 ty2) = canonicalize ty1 ++ canonicalize ty2
canonicalize ty = [ty]

compose :: [Type] -> Type
compose (tx:ty:tys) = TyArrow tx (compose $ ty : tys)
compose (tx:[])     = tx
compose []          = error "can't compose empty type"

