module Sample where

type Person =
  { name :: String
  , age :: Int
  }

alice :: Person
alice =
  { name: "Alice"
  , age: 15
  }

f :: String -> Int -> Person
f name age = { name, age }

g :: Person -> String
g = _.name

h :: forall r. { age :: Int | r } -> Int
h = _.age