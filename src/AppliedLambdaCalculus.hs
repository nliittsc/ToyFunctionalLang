module AppliedLambdaCalculus where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

{- | This is an application of an Applied (Impure) Lambda
Calculus for the intermediate representation of a functional
language.
-}


type Name = String

data Constant = Cint Int | Cbool Bool deriving (Show, Eq)

data UniOp = Not deriving (Show, Eq)

data BinOp = Plus | Minus | Mult | And | Or deriving (Show, Eq)

-- | Terms in our language
data Term
    = Var Name
    | Const Constant
    | PrimUniOp UniOp Term
    | PrimBinOp BinOp Term Term
    | Lam Name Term
    | Applic Term Term
    deriving (Show, Eq)

    
data Value
    = Vint Int
    | Vbool Bool
    | VAbs Name Term

-- | Find the free variables in a term
freeVars :: Term -> Set.Set Name
freeVars (Const _) = Set.empty
freeVars (PrimUniOp _ _) = Set.empty
freeVars PrimBinOp {} = Set.empty
freeVars (Var x) = Set.singleton x
freeVars (Lam x t) = freeVars t `Set.difference` Set.singleton x
freeVars (Applic t1 t2) = freeVars t1 `Set.union` freeVars t2


-- | Get a fresh variable name
fresh :: Name -> Set.Set Name -> Name
fresh x fV
    | not (x `Set.member` fV) = x
    | otherwise               = fresh (x ++ "'") fV

-- | hopefully correct alpha conversion
subst :: Name -> Term -> Term -> Term
subst _ _ (Const c) = Const c
subst _ _ (PrimUniOp o (Const c)) = PrimUniOp o (Const c)
subst v f (PrimUniOp o v') = PrimUniOp o (subst v f v')
subst _ _ (PrimBinOp o (Const c) (Const c')) = PrimBinOp o (Const c) (Const c')
subst v f (PrimBinOp o (Const c) v') = PrimBinOp o (Const c) (subst v f v')
subst v f (PrimBinOp o v' v'') = PrimBinOp o (subst v f v') (subst v f v'')
subst v f (Var v')
    | v == v'   = f
    | otherwise = Var v'
subst v f (Applic e1 e2) = Applic (subst v f e1) (subst v f e2)
subst v f (Lam x e)
    | v == x = Lam x e
    | v /= x && not (x `Set.member` freeVars f) = Lam x (subst v f e)
    | otherwise = Lam y (subst v f e')
        where
            y = fresh x (freeVars e `Set.union` freeVars f)
            e' = subst x (Var y) e


deltaReduce1 Not (Cbool True) = Const (Cbool False)
deltaReduce1 Not (Cbool False) = Const (Cbool True)
deltaReduce1 _ _ = error "Bad delta reduce on Uniop"

deltaReduce2 Plus (Cint n) (Cint m) = Const (Cint (n + m))
deltaReduce2 Minus (Cint n) (Cint m) = Const (Cint (n - m))
deltaReduce2 Mult (Cint n) (Cint m) = Const (Cint (n * m))
deltaReduce2 And (Cbool b) (Cbool b') = Const (Cbool (b && b'))
deltaReduce2 Or (Cbool b) (Cbool b') = Const (Cbool (b || b'))
deltaReduce2 _ _ _ = error "bad delta reduce on Binop"

-- | δ-Reduction
reduce (Const c) = Const c
reduce (PrimUniOp o (Const c)) = deltaReduce1 o c
reduce (PrimBinOp o (Const c) (Const c')) = deltaReduce2 o c c'
-- | η-Reduction
reduce (PrimUniOp o t) = PrimUniOp o (reduce t)
reduce (PrimBinOp o t t') = PrimBinOp o (reduce t) (reduce t')
reduce (Var x) = Var x
reduce (Lam x e) = Lam x e
reduce (Applic t u) = case reduce t of
    Lam x t' -> subst x (reduce u) t'
    t        -> Applic t (reduce u)
    
normalize t =
    let u = reduce t
    in if t == u then t else normalize u

-- | test: This should reduce to "y"
testTerm1 = Applic (Lam "x" (Applic (Var "x") (Var "y"))) (Lam "x" (Var "x"))
-- >>> normalize testTerm1
-- Var "y"

testTerm2 = Applic (Lam "x" (PrimBinOp Plus (Var "x") (Const (Cint 3)))) (Const (Cint 1))

-- >>> normalize testTerm2
-- Const (Cint 4)

