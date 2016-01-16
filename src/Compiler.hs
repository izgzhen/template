{-# LANGUAGE LambdaCase #-}

module Compiler (
  compile
, runCompile
) where

import Prelude hiding (lookup)
import qualified Data.Map as M
import Control.Monad.State
import Debug.Trace

import AST

data Env = Env {
    _vals    :: M.Map Name (Compiler Term),
    _counter :: Int
}

initCompileEnv :: Env
initCompileEnv = Env {
  _vals    = stdlib
, _counter = 0
}

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

lookup :: Name -> String -> Compiler Term
lookup x info = do
    mTm <- M.lookup x <$> vals
    keys <- M.keys <$> vals
    case mTm of
        Nothing -> error $ "no such var: " ++ x ++ " in " ++ show keys
        Just tm -> tm

-- Stage One

runCompile :: Compiler Term -> Term
runCompile = flip evalState initCompileEnv

compile :: Term -> Compiler Term
compile t@(TmApp tm1 tm2)  = traceTm t $ TmApp <$> compile tm1 <*> compile tm2
compile t@(TmAbs x tyx tm) = traceTm t $ TmAbs x tyx <$> compile tm
compile t@(TmSplice tm)    = traceTm t $ evaluate tm
compile t@(TmBracket _)    = traceTm t $ error "Bracket in common term"
compile t@(TmTm _)         = traceTm t $ error "TmTm in common term"
compile t                  = traceTm t $ return t

traceTm :: Term -> a -> a
traceTm t  = trace ("compiling " ++ pprintTm t)

traceTm' :: Term -> a -> a
traceTm' t = trace ("evaluating " ++ pprintTm t)

evaluate :: Term -> Compiler Term
evaluate t@(TmApp tmf tmx) = traceTm' t $ do
    tmf' <- evaluate tmf
    tmx' <- evaluate tmx
    case traceTm' (TmApp tmf' tmx') tmf' of
        TmAbs x _ tm -> do
            let tm' = substIn x tmx' tm
            vals' <- vals
            evaluate $ trace ("Vals: " ++ show (M.keys vals')) tm'
        other -> error $ show other ++ " is not applicable"

evaluate t@(TmBracket tm) = traceTm' t $ TmBracket <$> compile tm
evaluate t@(TmSplice tm)  = traceTm' t $ evaluate tm
evaluate t@(TmVar x)      = traceTm' t $ lookup x "evaluate TmVar" >>= evaluate
evaluate t@(TmTm tmTerm)  = traceTm' t $ evaluateTm tmTerm
evaluate tm               = traceTm' tm $ return tm

evaluateTm :: TmTerm -> Compiler Term
evaluateTm (TmTmInt tm) = evaluate tm >>= \case
    TmInt i -> return $ TmInt i
    _ -> error $ show tm ++ " is not Int"

evaluateTm (TmTmString tm) = evaluate tm >>= \case
    TmString s -> return $ TmString s
    _ -> error $ show tm ++ " is not String"

evaluateTm (TmTmVar tm) = evaluate tm >>= \case
    TmString s -> return $ TmVar s
    other      -> error $ show other ++ " is not TmVar"

evaluateTm (TmTmApp tm1 tm2) = do
    tm1' <- evaluate tm1
    tm2' <- evaluate tm2
    return $ TmApp tm1' tm2'

evaluateTm (TmTmAbs tm1 tm2 tm3) = do
    tm1' <- evaluate tm1
    tm2' <- evaluate tm2
    tm3' <- evaluate tm3
    case (tm1', tm2', tm3') of
        (TmString x, TmType ty, tm)
          -> return $ TmAbs x ty tm
        _ -> error $ show "Illegal TmTmAbs"

--- Stdlib
stdlib :: M.Map Name (Compiler Term)
stdlib = M.fromList [
              ("genstr", TmString <$> genstr)
            , ("TmAbs", tmAbs)
            , ("TmVar", tmVar)
            , ("TyInt", tyInt)
            ]

genstr :: Compiler String
genstr = do
    i <- _counter <$> get
    let ret = "_x" ++ show i
    modify (\s -> s { _counter = i + 1 })
    return ret

tmAbs :: Compiler Term
tmAbs = do
    xname <- genstr
    xtype <- genstr
    xbody <- genstr
    return $ TmAbs xname TyString
                $ TmAbs xtype TyType
                    $ TmAbs xbody TyQ
                        $ TmTm $ TmTmAbs (TmVar xname) (TmVar xtype) (TmVar xbody)

tmVar :: Compiler Term
tmVar = do
    xname <- genstr
    return $ TmAbs xname TyString (TmVar xname)

tyInt :: Compiler Term
tyInt = return $ TmType TyInt

substIn :: Name -> Term -> Term -> Term
substIn x tmx = \case
    TmVar y | x == y    -> tmx
            | otherwise -> TmVar y
    TmApp tm1 tm2       -> TmApp (subst tm1) (subst tm2)
    t@(TmAbs y tyy tm)
            | x == y    -> t
            | otherwise -> TmAbs y tyy (subst tm)
    TmSplice _          -> error "splicing in splice"
    TmTm tmTerm         -> TmTm $ substIn' x tmx tmTerm
    other               -> other
  where
    subst = substIn x tmx

substIn' :: Name -> Term -> TmTerm -> TmTerm
substIn' x tmx = \case
    TmTmInt tm      -> TmTmInt $ subst tm
    TmTmString tm   -> TmTmString $ subst tm
    TmTmVar tm      -> TmTmVar $ subst tm
    TmTmApp tm1 tm2 -> TmTmApp (subst tm1) (subst tm2)
    TmTmAbs tm1 tm2 tm3 -> TmTmAbs (subst tm1) (subst tm2) (subst tm3)
  where
    subst = substIn x tmx

