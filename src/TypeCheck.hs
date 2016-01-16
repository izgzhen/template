module TypeCheck (
  typeCheckTerm
, initTyEnv
, unify
) where

import AST
import Data.Maybe (maybe)
import Data.List (stripPrefix, tails)
import qualified Data.Map as M

data Env = Env {
  _mapSplice  :: M.Map Name Type
, _mapBracket :: M.Map Name Type
}

initTyEnv :: Env
initTyEnv = Env {
  _mapSplice  = stdlib
, _mapBracket = stdlibUser
}


typeCheckTerm :: Env -> Term -> Either String Type
typeCheckTerm env tm = typeCheckBracket env tm

-- Stage 1 type Check

-- Typing in general

typeCheck :: Env -> Term ->
             (Name -> Type -> Env -> Env) ->
             (Name -> Env -> Maybe Type) ->
             (Env -> Term -> Either String Type) ->
             Either String Type
typeCheck env tm insert query cont = typeCheck' tm
    where
        typeCheck' (TmInt _)    = return TyInt
        typeCheck' (TmString _) = return TyString

        typeCheck' (TmVar x) = maybe (Left $ "no such var: " ++ x) Right $ query x env

        typeCheck' (TmApp tm1 tm2) = do
            ty1 <- cont env tm1
            ty2 <- cont env tm2
            case unify ty2 ty1 of
                LLeft ty3 -> Right ty3
                _         -> Left $ "Illegal type for TmApp: " ++ show tm1 ++ " : " ++ show ty1 ++
                                    " and " ++ show tm2 ++ " : " ++ show ty2

        typeCheck' (TmAbs x ty tm) = do
            tytm <- cont (insert x ty env) tm
            return $ TyArrow ty tytm

-- Typing in bracket

typeCheckBracket env (TmSplice tm) = do
    tytm <- typeCheckSplice env tm
    return TyBottom

typeCheckBracket env (TmBracket _) = Left "TmBracket in bracket"
typeCheckBracket env (TmType _)    = Left "TmType in bracket"
typeCheckBracket env (TmTm _)      = Left "TmTm in bracket"
typeCheckBracket env other         = typeCheck env other insert query typeCheckBracket
    where
        query  k e   = M.lookup k (_mapBracket e)
        insert k v e = e { _mapBracket = M.insert k v $ _mapBracket e }


-- Typing in splice

typeCheckSplice env (TmSplice tm)  = Left "TmSplice in splice"
typeCheckSplice env (TmBracket tm) = typeCheckBracket env tm >> return TyQ
typeCheckSplice env (TmType _)     = return TyType
typeCheckSplice env (TmTm tmTerm)  = return TyQ
typeCheckSplice env other          = typeCheck env other insert query typeCheckSplice
    where
        query  k e   = M.lookup k (_mapSplice e)
        insert k v e = e { _mapSplice = M.insert k v $ _mapSplice e }

-- Stdlib interface

stdlib = M.fromList [
              ("genstr", TyString)
            , ("TmAbs", TyArrow (TyArrow (TyArrow TyString TyType) TyQ) TyQ)
            , ("TmVar", TyArrow TyString TyQ)
            , ("TyInt", TyType)
            ]

stdlibUser = M.fromList [
              ("plus", TyArrow (TyArrow TyInt TyInt) TyInt)
            ]

-- Helpers

canonicalize :: Type -> [Type]
canonicalize (TyArrow ty1 ty2) = canonicalize ty1 ++ canonicalize ty2
canonicalize ty = [ty]

compose :: [Type] -> Type
compose (tx:ty:tys) = TyArrow tx (compose $ ty : tys)
compose (tx:[])     = tx
compose []          = error "can't compose empty type"

data Result = LLeft Type
            | RLeft Type
            | NonLeft
            | Failure
            deriving (Eq, Show)

unify :: Type -> Type -> Result
unify ty1 ty2 = unify' (canonicalize ty1) (canonicalize ty2)

unify' :: [Type] -> [Type] -> Result
unify' (TyBottom : xs) yss@(y:ys) =
    case filter (/= Failure) [ unify' xs tl | tl <- tail $ tails yss ] of
        []    -> Failure
        (x:_) -> x

unify' xss@(x:xs) (TyBottom : ys) =
    case filter (/= Failure) [ unify' tl ys | tl <- tail $ tails xss ] of
        []    -> Failure
        (x:_) -> x

unify' (x:xs) (y:ys)
    | x == y    = unify' xs ys
    | otherwise = Failure

unify' [] [] = NonLeft
unify' [] ys = RLeft $ compose ys
unify' xs [] = LLeft $ compose xs

