module Sample where

greaterThan :: Int -> Int -> Boolean
greaterThan _ _ = true

infix 5 greaterThan as >

lessThan :: Int -> Int -> Boolean
lessThan _ _ = true

infix 5 lessThan as <

eq :: Int -> Int -> Boolean
eq _ _ = true

infixl 5 eq as ==

mod :: Int -> Int -> Int
mod _ _ = 0

f :: Int -> Int -> Int
f m n = case m, n of
  m', _ | m' > 0, n > 0 -> 1
  1, n' -> 2
  2, n' | n' < 0 -> 3
  m', n'
    | m' `mod` n' == 0 -> 3
  _, n' -> 4