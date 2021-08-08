module Syntax where


data Expr
    = EVar String
    | EAbs String Expr
    | EApp Expr Expr