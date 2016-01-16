module AST where

type Name = String

data Term = TmInt Int
          | TmString String
          | TmVar Name
          | TmApp Term Term
          | TmAbs Name Type Term
          -- Next nodes are stage 1 only
          | TmSplice Term
          | TmBracket Term
          | TmType Type
          | TmTm TmTerm
          deriving (Eq, Show)

data TmTerm = TmTmInt Term
            | TmTmString Term
            | TmTmVar Term
            | TmTmApp Term Term
            | TmTmAbs Term Term Term
            deriving (Show, Eq)

data Type = TyInt
          | TyString
          | TyArrow Type Type
          -- Next nodes are stage 1 only
          | TyBottom
          | TyType
          | TyQ
          deriving (Eq, Show)

pprintTy :: Type -> String
pprintTy TyInt    = "Int"
pprintTy TyString = "String"
pprintTy (TyArrow ty1 ty2) = "(" ++ pprintTy ty1 ++ " -> " ++ pprintTy ty2 ++ ")"
pprintTy TyQ      = "Q"
pprintTy TyBottom = "⊥"
pprintTy TyType   = "TyType"


pprintTm :: Term -> String
pprintTm (TmInt i)    = show i
pprintTm (TmString s) = show s
pprintTm (TmVar name) = name
pprintTm (TmApp tm1 tm2) = "(" ++ pprintTm tm1 ++ ") (" ++ pprintTm tm2 ++ ")"
pprintTm (TmAbs x tyx tm) = "λ" ++ x ++ ":" ++ pprintTy tyx ++ ". " ++ pprintTm tm
pprintTm (TmSplice tm) = "$(" ++ pprintTm tm ++ ")"
pprintTm (TmBracket tm) = "[| " ++ pprintTm tm ++ "|]"
pprintTm (TmType ty) = show ty
pprintTm (TmTm tm) = show tm 

