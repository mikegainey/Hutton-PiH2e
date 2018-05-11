-- Graham Hutton: Programming in Haskell
-- Chapter 6 exercises

{- 1. How does the recursive version of the factorial function behave if appied to a negative argument, such as (-1)?
 Modify the definition to prohibit negative arguments by adding a guard to the recursive case.

Ans: An infinite loop results.

-}

fac 0 = 1
fac n = n * fac (n-1)

fac' 0 = 1
fac' n
  | n > 0 = n * fac' (n-1)
  | otherwise = 1

{- 2. Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given
 value down to zero. For example, sumdown 3 should return the result 3+2+1+0 = 6.  -}

sumdown 0 = 0
sumdown n = n + sumdown (n-1)

{- 3. Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the
 multiplication operator *, and show how the expression 2 ^ 3 is evaluated using your definition. -}

m ^! 0 = 1
m ^! n = m * (m ^! (n-1))

{- 4. Define a recursive function euclid :: Int -> Int -> Int that implements Euclid's algorithm for calculating the
 greatest common divisor of two non-negative integers: if the two numbers are equal, this number is the result;
 otherwise, the smaller number is subtracted from the larger, and the same process is then repeated. For example:
> euclid 6 27
3
-}

euclid a b
  | a == b = a
  | otherwise = euclid smaller larger
  where smaller = min a b
        larger = (max a b) - smaller

-- book answer
euclid' x y | x == y = x
           | x < y = euclid' x (y-x)
           | y < x = euclid' (x-y) y

{- 5. Using the recursive definitions given in this chapter, show how length [1,2,3], drop 3 [1,2,3,4,5], and init [1,2,3]
 are evaluated.

length []     = 0
length (_:xs) = 1 + length xs

length [1,2,3] -->
1 + length [2,3] -->
1 + 2 + length [3] -->
1 + 2 + 3 + length [] -->
1 + 2 + 3 + 0 -->
6

drop 0 xs     = xs
drop _ []     = []
drop n (_:xs) = drop (n-1) xs

drop 3 [1,2,3,4,5] -->
drop 2 [2,3,4,5] -->
drop 1 [3,4,5] -->
drop 0 [4,5] -->
[4,5]

init (x:xs)
  | null xs = []
  | otherwise = x : init xs

init [1,2,3] -->
1 : init [2,3] -->
1 : 2 : init [3] -->
1 : 2 : []
-}

-- 6. Define the following library functions on lists using recursion:

-- 6a. Decide if all logical values in a list are True:
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- 6b. Concatenate a list of lists:
concat' :: [[a]] -> [a]
concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

-- 6c. Produce a list with n identical elements:
replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n-1) a

-- 6d. Select the nth element of a list:
(!!!) :: [a] -> Int -> a
(!!!) (x:xs) 0 = x
(!!!) (x:xs) n = (!!!) xs (n-1)

-- 6e. Decide if a value in an element of a list:
elem' :: Eq a => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs) = (a == x) || elem' a xs

{- 7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted
 list. For example:
> merge [2,5,6] [1,3,4]
[1,2,3,4,5,6]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | x <= y    = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

{- 8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and
 singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from
 sorting the two halves of the list separately. Hint: first define a function halve :: [a] -> ([a],[a]) that splits a
 list into two halves whose lengths differ by at most one.
-}

msort []  = []
msort [x] = [x]
msort xs  = merge (msort firsthalf) (msort secondhalf)
  where firsthalf  = take n xs
        secondhalf = drop n xs
        n = length xs `div` 2

-- 9. Using the five-step process, construct the library functions that:

-- 9a. calculate the sum of a list of numbers
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


-- 9b. take a given number of elements from the start of a list:
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

-- 9c. select the last element of a non-empty list:
last' :: [a] -> a
last' (x:[]) = x
last' (x:xs) = last' xs

