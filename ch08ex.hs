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

{- 2. Although not included n appendixB, the standard prelude defines

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

-- balancedTree :: Tree
-- balancedTree ==
-- unbalancedTree :: Tree
-- unbalancedTree

numLeaves :: Tree a -> Int
numLeaves (Leaf _) = 1
numLeaves (Node left right) = numLeaves left + numLeaves right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left right) = ((abs (numLeaves left - numLeaves right)) < 2) && balanced left && balanced right

{- 4. Define a function balance :: [a] -> Tree a that converts a non-empty list into a balanced tree. Hint: first define a
 function that splits a list into two halves wholse length differs by at most one. -}


