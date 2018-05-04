-- Graham Hutton: Programming in Haskell
-- Chapter 1 exercises

{-

1. Give another possible calculation for the result of:  double (double 2):

function:  double x = x + x

double (double 2)
= {applying the inner double}   double (2 + 2)
= {applying the other double}   (2 + 2) + (2 + 2)
= {applying the left +}         4 + (2 + 2)
= {applying the right +}        4 + 4
= {applying the remaining +}    8

2. Show that sum [x] = x for any number x

definition:
sum [] = 0
sum (n:ns) = n + sum ns

sum (x:[])
= {applying sum}  x + sum []
= {applying sum}  x + 0
= {applying +}    x

-}

product' [] = 1
product' (n:ns) = n * product' ns

{- 3.  Define a product function

product' [2,3,4]
= {applying product'}
  2 * product' [3,4]
= {applying product'}
  2 * 3 * product' [4]
= {applying product'}
  2 * 3 * 4 * product' []
= {applying product'}
  2 * 3 * 4 * 1
= {applying *}
  24

-}

-- 4. reverse qsort

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ a | a <- xs, a <= x]
        larger = [ b | b <- xs, b > x]

revqsort [] = []
revqsort (x:xs) = revqsort larger ++ [x] ++ revqsort smaller
  where smaller = [ a | a <- xs, a <= x]
        larger = [ b | b <- xs, b > x]

-- 5. What would be the effect of replacing <= with < in qsort?
-- Consider qsort [2,2,3,1,1]
-- My guess: duplicate values disappear            yes!
--           so, qsort [2,2,3,1,1] --> [1,2,3]

qsort' [] = []
qsort' (x:xs) = qsort' smaller ++ [x] ++ qsort' larger
  where smaller = [ a | a <- xs, a < x]
        larger = [ b | b <- xs, b > x]
