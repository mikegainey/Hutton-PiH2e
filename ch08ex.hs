-- Graham Hutton: Programming in Haskell
-- Chapter 8 exercises

{- 1. In a similar manner to the function add, define a recursive multiplication function mult :: Nat -> Nat -> Nat for
 the recursive type of natural numbers. Hint: make use of add in your definition. -}

data Nat = Zero | Succ Nat
  deriving (Show)

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero x = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult x Zero = Zero
mult x (Succ y) = add x (mult x y)

