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

map' f = foldr (\x acc -> f x : acc) []

filter' p = foldr (\e acc -> if p e then e : acc else acc) []

{- 4. Using foldl, define a function dec2int :: [Int] -> Int that converts a decimal number into an integer. For example:
> dec2int [2,3,4,5]
2345  -}

dec2int :: [Int] -> Int
dec2int = foldl (\acc d -> d + acc*10) 0

{- 5. Without looking at the definitions from the standard prelude, define the higher-order library function curry that
 converts a function on pairs into a curried function, and, conversely, the function uncurry that converts a curried
 function with two arguments into a function on pairs. Hint: first write down the types of the two functions.  -}

-- my version
curry' :: ((a,b) -> c) -> a -> b -> c
curry' f = \a b -> f (a,b)

-- in the Prelude
curry'' :: ((a,b) -> c) -> a -> b -> c
curry'' f x y = f (x,y)

-- my version
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(a,b) -> f a b

-- in the Prelude
uncurry'' :: (a -> b -> c) -> ((a,b) -> c)
uncurry'' f p = f (fst p) (snd p)

{- 6. A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

That is, the function unfold p h t produces the empty list if the predicate p is true of the argument value, and
otherwise produces a non-empty list by applying the function h to this value to give the head, and the function t to
produce the tail of the list. For example, the funciton int2bin can be rewritten more compactly using unfold as follows:

int2bin = unfold (== 0) (`mod` 2) (`div` 2)

Redefine the functions chop8, map f and iterate f using unfold.  -}

unfold p h t x
  | p x = []
  | otherwise = h x : unfold p h t (t x)

-- 6a. chop8; chops a list up into 8-element sub lists; the list has a length that is a multiple of 8
chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

-- 6b. map f
map'' f = unfold null (f . head) tail

-- 6c. iterate f
iterate' f = unfold (\x -> False) id f

 -- 7 & 8. See BinaryStringParity.hs

{- 9. Define a function altMap :: (a -> b) -> (a -> b) -> [a] -> [b] that alternately applies its two argument functions
 to successive elements in a list, in turn about order. For example:
> altMap (+10) (+100) [0,1,2,3,4]
[10,101,12,103,14]  -}

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 [] = []
altMap f1 f2 [z] = f1 z : []
altMap f1 f2 (x:y:zs) = f1 x : f2 y : altMap f1 f2 zs

{- 10. Using altMap, define a function luhn :: [Int] -> Bool that implements the Luhn algorithm from the exercises in
 chapter 4 for bank card numbers of any length. Test your new function using your own bank card.  -}

-- luhn :: [Int] -> Bool
luhn = div10 . sum . sub9 . doubled
  where doubled = reverse . altMap id (*2) . reverse
        sub9 = map (\x -> if x > 9 then x-9 else x)
        div10 x = x `mod` 10 == 0
