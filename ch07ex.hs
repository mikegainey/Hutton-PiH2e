-- Graham Hutton: Programming in Haskell
-- Chapter 7 exercises

{- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and
 filter.

Ans: That list comprehension selects only the elements from xs that satisfy predicate p: (filter p xs). Function f is
then applied to each element of the resulting list: (map f . filter p) xs. -}

{- 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on
 lists.  -}

-- 2a. Decide if all elements of a list satisfies a predicate:
all' :: ( a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = p x && all' p xs


-- 2b. Decide if any element of a list satisfies a predicate:
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = p x || any' p xs

-- 2c. Select elements from a list while they satisfy a predicate:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

-- 2d. Remove elements from a list while they satisfy a predicate:
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs

-- 3. Redefine the functions map f and filter p using foldr.

map' f = foldr (\e acc -> f e : acc) []

filter' p = foldr (\e acc -> if p e then e : acc else acc) []

{- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
> dec2int [2,3,4,5]
2345  -}

dec2int :: [Int] -> Int
dec2int = foldl (\acc d -> d + acc*10) 0

