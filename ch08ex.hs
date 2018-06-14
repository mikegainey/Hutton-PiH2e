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
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

add :: Nat -> Nat -> Nat
add Zero x     = x
add (Succ x) y = Succ (add x y)

mult :: Nat -> Nat -> Nat
mult x Zero     = Zero
mult x (Succ y) = add x (mult x y)

{- 2. Although not included n appendix B, the standard prelude defines

     data Ordering - LT | EQ | GT

together with a function

     compare :: Ord a => a -> a -> Ordering

  that decides if one value in an ordered type is less than (LT), equal to (EQ), or greater than (GT) another value.
  Using this function, redefine the function occurs :: Ord a => a -> Tree -> Bool for search trees. Why is this new
  definition more efficient than the original version? -}

data ATree a = ALeaf a | ANode (ATree a) a (ATree a)
  deriving (Show)

testATree :: ATree Int
testATree = ANode (ANode (ALeaf 1) 3 (ALeaf 4)) 5 (ANode (ALeaf 6) 7 (ALeaf 9))

occurs :: Ord a => a -> ATree a -> Bool
occurs x (ALeaf y) = (x == y)
occurs x (ANode left y right)
  | x == y = True
  | x < y = occurs x left
  | x > y = occurs x right
  | otherwise = error "This should never happen"

occurs' :: Ord a => a -> ATree a -> Bool
occurs' x (ALeaf y) = (x == y)
occurs' x (ANode left y right) = case (compare x y) of
  EQ -> True
  LT -> occurs' x left
  GT -> occurs' x right

-- occurs' is more efficient because only 1 compare is done instead of up to 3 in the function using guards.

{- 3. Consider the following type of binary trees:

data Tree a = Leaf a | Node (Tree a) (Tree a)

Let us say that such a tree is balanced if the number of leaves in the left and right subtree of every node differs by
at most one, with leaves themselves being trivially balanced. Define a function balanced :: Tree a -> Bool that decides
if a binary tree is balanced or not. Hint: first define a function that returns the number of leaves in a tree. -}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node left right) = numLeaves left + numLeaves right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = ((abs (numLeaves left - numLeaves right)) <= 1) && balanced left && balanced right

{- 4. Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree. Hint: first define a
 function that splits a list into two halves whose length differs by at most one. -}

splitList xs = (take half xs, drop half xs)
  where half = (length xs) `div` 2

balance :: [a] -> Tree a
balance (x:[]) = Leaf x
balance xs = Node (balance left) (balance right)
  where left  = (fst . splitList) xs
        right = (snd . splitList) xs

{- 5. Given the type declaration

     data Expr = Val Int | Add Expr Expr

define a higher-order function

     folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a

such that folde f g replaces each Val constructor in an expression by the function f, and each Add constructor by the
function g. -}

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

{- 6. Using folde, define a function eval :: Expr -> Int that evaluates an expression to an integer value, and a function
 size :: Expr -> that calculates the number of values in an expression. -}

eval :: Expr -> Int
eval exp = folde id (+) exp

size :: Expr -> Int
size exp = folde (\x -> 1) (+) exp

-- 7. TODO: Complete the following instance declarations:

-- instance Eq a => Eq (Maybe a) where

-- instance Eq a => Eq [a] where

-- 8 & 9: see TautologyExtended.hs and AbstractMachineExtended.hs
