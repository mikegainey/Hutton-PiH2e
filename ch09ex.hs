-- Graham Hutton: Programming in Haskell
-- Chapter 9 exercises

{- 1. Redefine the combinatorial function choices using a list comprehension rather than using composition, concat and
 map. -}

subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

choices' xs = [zs | ys <- subs xs, zs <- perms ys]

{- 2. Define a recursive function isChoice :: Eq a => [a] -> [a] -> Bool that decides if one list is chosen from another,
 without using the combinatorial functions perms and subs. Hint: start by defining a function that removes the first
 occurrence of a value from a list. -}

remove :: Eq a => a -> [a] -> [a]
remove a [] = []
remove a (x:xs)
  | x == a = xs
  | otherwise = x : (remove a xs)

-- Is the first argument chosen from the second?
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys = (x `elem` ys) && isChoice xs (remove x ys)

{- 3. What effect would generalizing the function split to also return pairs containing the empty list have on the
 behavior of solutions?

Ans: I don't know. After trying split'', it seems to create an endless loop.

Book answer: It would lead to non-termination, because recursive calls to exprs would no longer be guaranteed to reduce
the length of the list. -}

split'' :: [a] -> [([a], [a])]
split'' xs = [(take n xs, drop n xs) | n <- [0..(length xs)]]

{- 4. Using the functions choices, exprs, and eval, verify that there are 33,665,406 possible expressions over the numbers
 1, 3, 7, 10, 25, 50, and that only 4,672,540 of these expressions evaluate successfully.

*Main> length $ concat $ map exprs $ choices [1,3,7,10,25,50]
33665406 (verified)
*Main> length $ filter (\e -> (eval e) /= []) $ concat $ map exprs $ choices [1,3,7,10,25,50]
4672540 (verified) -- with the old valid function
*Main> length $ filter (\e -> (eval e) /= []) $ concat $ map exprs $ choices [1,3,7,10,25,50]
245644 -- with the new valid function -}

{- 5. Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369 if the numeric
 domain is generalized to arbitrary integers. Hint: modify the definition of valid.

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = True
valid Mul _ _ = True
valid Div x y = y /=  0 && x `mod` y == 0

*Main> length $ filter (\e -> (eval e) /= []) $ concat $ map exprs $ choices [1,3,7,10,25,50]
10839369
-}

{- 6. Modify the final program to:

6a. allow the use of exponentiation in expressions;

6b. produce the nearest solutions if no exact solution is possible;

6c. order the solutions using a suitable measure of simplicity;

Ans: see Countdown2.hs

-}
