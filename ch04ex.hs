-- Graham Hutton: Programming in Haskell
-- Chapter 4 exercises

-- 1. Using library functions, define a function halve :: [a] -> ([a], [a]) that splits an even-lengthed list into two halves:
-- > halve [1,2,3,4,5,6]
-- ([1,2,3], [4,5,6])

halve lst = (firsthalf, secondhalf)
  where len        = length lst
        halflen    = len `div` 2
        firsthalf  = take halflen lst
        secondhalf = drop halflen lst

-- 2. Define a function third :: [a] -> a that returns the third element in a list that contains at least this many elements:

-- 2a. using head and tail
third' xs = head (tail (tail xs))

-- 2b. using list indexing (!!)
third'' xs = xs !! 2

-- 2c. using pattern matching
third''' (_:_:c:_) = c

-- 3. Consider a function safetail :: [a] -> [a] that behaves in the same way as tail except that it maps the empty list
-- to itself rather than producing an error. Using tail and the function null :: [a] -> Bool that decides if a list is
-- empty or not, define safetail using:

-- 3a. a conditional expression
safetail' xs = if null xs then [] else tail xs

-- 3b. guarded equations
safetail'' xs | null xs   = []
              | otherwise = tail xs

-- 3c. pattern matching
safetail''' []     = []
safetail''' (x:xs) = xs

-- 4. In a similar way to && in section 4.4, show how the disjuntion operator || can be defined in four different ways
-- using pattern matching.

True  ||! True  = True
True  ||! False = True
False ||! True  = True
False ||! False = False

False ||@ False = False
_     ||@ _     = True

False ||# b = b
_     ||# _ = True

b ||$ c | b == c    = b
        | otherwise = True

-- 5. Without using any other library functions or operators, show how the meaning of the following pattern matching
-- definition for logical conjunction && can be formalised using conditional expressions:
-- True && True = True
-- _    && _    = False

b &&! c = if b == True
          then if c == True
               then True
               else False
          else False
