module LambdaCalculus where

import Control.Monad

-- | Follows the ML implementation presented in Benjamin Pierce

-- | Terms using De Bruijn Indices. Ints represent variables
data Term =
      Var Int Int      -- Second Int used for debugging
    | Abs String Term  -- String is used for printing
    | App Term Term
    deriving (Show, Eq)



-- | Shifting helper function. `d` is shift amount, `c` is cutoff
shift :: Int -> Int -> Term -> Term
shift d c (Var k n)
    | c < k     = Var k (n + d)
    | otherwise = Var (k + d) (n + d)
shift d c (Abs x t) = Abs x (shift d (c+1) t)
shift d c (App t1 t2) = App (shift d c t1) (shift d c t2)

-- | Substitution of j for s in [j/s]t
subst :: Int -> Term -> Term -> Term
subst j s (Var k n)
    | k == j    = s
    | otherwise = Var k n
subst j s (Abs x t) = Abs x (subst (j+1) s' t)
    where
        s' = shift 1 0 s
subst j s (App t1 t2) = App (subst j s t1) (subst j s t2)

-- | Top level shift, to take in terms
substTop :: Term -> Term -> Term
substTop s t = shift (-1) 0 (subst 0 (shift 1 0 s) t)

-- | Evaluation Stuff

data Binding = NameBind

type Context = [(String, Binding)]

-- | Some helpers to do printing

indexToName ctx i = fst (ctx !! i)

-- | looks in the context for a variable with the same name
getFresh [] x = x
getFresh ((y, nb) : ys) x
    | head x == head y && length x <= length y = getFresh ((y, nb) : ys) (x ++ "'")
    | otherwise = getFresh ys x

-- | Get a fresh variable name
pickFreshName ctx x
    | x' == x   = (ctx, x)
    | otherwise = (ctx', x')
      where
          x' = getFresh ctx x
          ctx' = (x', NameBind) : ctx


termToString ctx (Abs x t) =
    let (ctx', x') = pickFreshName ctx x
    in "(λ" ++ x' ++ "." ++ termToString ctx' t
termToString ctx (App t1 t2) = 
    let t1Str = termToString ctx t1
        t2Str = termToString ctx t2
    in "( " ++ t1Str ++ " " ++ t2Str ++ " )"
termToString ctx (Var x n)
    | length ctx == n = indexToName ctx x
    | otherwise       = "[bad index]"



-- | Checks if a term is a value, given some context
isVal ctx (Abs _ _) = True
isVal ctx _         = False

-- | Pattern match on beta-reduce, Eapp2, Eapp1
eval' ctx (App (Abs x t12) v2)
    | isVal ctx v2 = Just (substTop v2 t12)
eval' ctx (App v1 t2)
    | isVal ctx v1 = fmap (App v1) (eval' ctx t2)
eval' ctx (App t1 t2) = fmap (`App` t2) (eval' ctx t1)
eval' _ _ = Nothing

-- | Top level evaluation. If no rule applies then return the term.
eval ctx t = case eval' ctx t of
    Just t' -> eval ctx t'
    Nothing -> t


