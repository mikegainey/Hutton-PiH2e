-- Graham Hutton: Programming in Haskell
-- Chapter 2 exercises

-- 1. What are the types of the following values:

ex1a = ['a', 'b', 'c'] -- :: [Char]

ex1b = ('a', 'b', 'c') -- :: (Char, Char, Char)

ex1c = [(False, '0'), (True, '1')] -- :: [(Bool, Char)]

ex1d = ([False, True], ['0', '1']) -- :: ([Bool], [Char])

ex1e = [tail, init, reverse] -- :: [[a] -> [a]]

-- 2. Write down definitions that have the following types.  It doesn't matter what the definitions do.

bools :: [Bool]
bools = [True, False]

nums :: [[Int]]
nums = [[1,2,3], [4,5,6]]

add :: Int -> Int -> Int -> Int
add a b c = a + b + c

copy :: a -> (a,a)
copy x = (x,x)

apply :: (a -> b) -> a -> b
apply f x = f x

-- 3. What are the types of the following functions?

second :: [a] -> a
second xs = head (tail xs)

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

pair :: a -> b -> (a,b)
pair x y = (x,y)

double :: Num a => a -> a
double x = x*2

palindrome :: Eq a => [a] -> Bool -- missed the Eq a =>
palindrome xs = reverse xs == xs

-- twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- 4. Check your answers in GHCi

-- 5. Why is it not feasable in general for function types to be instances of the Eq class? When is it feasable?
-- Hint: two functions of the same type are equal if they always return equal results for equal arguments.

-- Ans: Haskell doesn't have a way to determine if two functions behave in the same way.  It is feasable when
-- the function bodies are equivalent. ? (not a good answer)
