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

h :: Array Int -> Int
h = case _ of
  [] -> 1
  [ 1 ] -> 2
  [ x, 1 ] -> 3
  [ 3 ] -> 4
  [ x, y, 2 ] -> 5
  [ x, _, y ] -> 6
  xs -> 7

type Person = { name :: String, age :: Int }

g :: Person -> Int
g = case _ of
  { name: "aice" } -> 0
  { name, age: 15 } -> 1
  { age } | age >= 18 -> 3
  _ -> 4

ge :: Int -> Int -> Boolean
ge _ _ = true

infix 8 ge as >=