-- Graham Hutton: Programming in Haskell
-- Chapter 12 exercises (mostly modifiying tic-tac-toe)

-- 1. Define an instance of the Functor class for the following type of binary trees that have data in their nodes:

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  fmap _ Leaf = Leaf
  fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)

t1 :: Tree Int
t1 = Node (Node Leaf 3 Leaf) 2 (Node (Node Leaf 5 Leaf) 4 Leaf)

{- 2. TODO: Complete the following instance declaration to make the partially-applied function type (a ->) into a functor.
 Hint: first write down the type of fmap, and then think if you already know a library function that has this type.

I don't understand the type (a ->).  -}

-- instance Functor ((->) a) where
  -- fmap :: Functor f => (a -> b) -> f a -> f b
  -- fmap g

{- TODO: exercises 3 to 8

I don't understand this chapter well enough to do these exercises. In fact, I'm having Haskell fatigue and need to give
this book a rest for now. -}

