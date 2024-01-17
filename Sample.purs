module Sample where

data Expr
  = Zero
  | Succ Expr
  | Add Expr Expr
  | Mul Expr Expr

f :: Expr -> Int
f = case _ of
  Add Zero Zero -> 1
  Mul Zero x -> 2
  Add (Succ x) y -> 3
  Mul x Zero -> 4
  Mul (Add x y) z -> 5
  Add x Zero -> 6
  x -> 7