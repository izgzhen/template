module AST where

type Name = String

data Term = TmInt Int
          | TmString String
          | TmVar Name
          | TmApp Term Term
          | TmAbs Name Type Term
          | TmSplice Term
          | TmBracket Term
          | TmTerm Term
          | TmType Type
          deriving (Show, Eq)

data Type = TyInt
          | TyString
          | TyArrow Type Type
          | TyQ
          | TyBottom
          | TyType
          | TyTmTerm
          deriving (Eq)

instance Show Type where
    show TyInt    = "Int"
    show TyString = "String"
    show (TyArrow ty1 ty2) = "(" ++ show ty1 ++ " -> " ++ show ty2 ++ ")"
    show TyQ      = "Q"
    show TyBottom = "⊥"
    show TyType   = "TyType"
    show TyTmTerm = "TyTmTerm"