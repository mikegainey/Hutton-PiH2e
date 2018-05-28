-- Graham Hutton: Programming in Haskell
-- recursion practice, chapter 6

-- factorial
fac 0 = 1
fac n = n * fac (n-1)

-- multiplication. It doesn't work when n is less than zero.
mul a 0 = 0
mul a n = a + mul a (n-1)

-- product of a list
product' [] = 1
product' (x:xs) = x * product' xs

-- length of a list
length' [] = 0
length' (_:xs) = 1 + length' xs

-- reverse a list
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- append two lists
append [] xs = xs
append (x:xs) (ys) = x : append xs ys

-- insert a value in a sorted list
insert a [] = [a]
insert a (x:xs)
  | a <= x    = a : x : xs
  | otherwise = x : insert a xs

-- insertion sort
isort [] = []
isort (x:xs) = insert x (isort xs)

-- zip; recursion on multiple arguments; has multiple base cases
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- drop
drop' 0 xs = xs
drop' _ [] = []
drop' n (x:xs) = drop' (n-1) xs

-- nth fibonacci number; multiple recursive calls in the body
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

-- quick sort
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = filter (<=x) xs
        larger = filter (>x) xs

-- isEven and isOdd; mutual recursion; a number is even if the previous number is odd
isEven 0 = True
isEven n = isOdd (n-1)

isOdd 0 = False
isOdd n = isEven (n-1)

-- evens and odds; return even or odd elements of a list
evens [] = []
evens (x:xs) = x : odds xs

odds [] = []
odds (_:xs) = evens xs

