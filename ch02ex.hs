-- Graham Hutton: Programming in Haskell
-- Chapter 2 exercises

-- 1. Work through examples in the chapter in GHCi.

-- 2. Parenthasize the following expressions:

a = 2^3*4   == (2^3)*4

b = 2*3+4*5 == (2*3) + (4*5)

c = 2+3*4^5 == 2+(3*(4^5))

-- 3. Correct the errors and verify that the script works properly in GHCi.

n = a `div` length xs  -- 10 `div` 5 --> 2
  where a = 10
        xs = [1,2,3,4,5]

-- 4. last selects the last element of a non-empty list. Show how last could be defined in terms of other library
-- functions introduced in this chapter.

last' xs = head (reverse xs)

last'' xs = xs !! (length xs - 1)

last''' xs = drop (length xs - 1) xs  -- returns a list, not an element

-- 5. init removes the last element from a non-empty list. Show how init could be similiarly defined in two different
-- ways.

init' xs = reverse (tail (reverse xs))

init'' xs = take (length xs - 1) xs
