-- Graham Hutton: Programming in Haskell
-- Chapter 10 exercises

import System.IO

{- 1. Redifine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ().
-}

putStr' :: String -> IO ()
putStr' xs = sequence_ [ putChar x | x <- xs]

{- 2. Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of any size, rather than
 being specific to boards with just five rows of stars. Hint: first define an auxiliary function that takes the current
 row number as an additional argument. -}

type Board = [Int]

putRow :: Show a => a -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- I couldn't do this on my own! Had to look it up!
putBoardR' :: (Num t, Show t) => [Int] -> t -> IO ()
putBoardR' [] r = return ()
putBoardR' (x:xs) r = do putRow r x
                         putBoardR' xs (r+1)

putBoardR :: [Int] -> IO ()
putBoardR b = putBoardR' b 1

{- 3. In a similar manner to the first exercise, redefine the generalized version of putBoard using a list comprehension
 and sequence_. -}

putBoardS :: [Int] -> IO ()
putBoardS b = sequence_ [putRow row num | (row,num) <- zip [1..] b ]

{- 4. Define an action adder :: IO () that reads a given number of integers from the keyboard, one per line, and displays
 their sum. Hint: start by defining an auxiliary function that takes the current total and how many numbers remain to be
 read as arguments. You will also likely need to use the library functions read and show. For example:

> adder
How many numbers? 5
1
3
5
7
9
The total is 25
-}

-- Got it!  All by myself!
adderR' :: (Num t, Eq t) => Int -> t -> IO Int
adderR' total 0 = return total
adderR' total remain = do xstr <- getLine
                          let x = read xstr :: Int
                          adderR' (total + x) (remain - 1)

adderR :: (Eq t, Num t) => t -> IO Int
adderR n = adderR' 0 n

{- 5. Redefine adder using the function sequence :: [IO a] -> IO [a] that performs a list of actions and returns a list of
 the resulting values. -}

-- I think he was expecting something else, since fmap hasn't been introduced yet.
adderS :: (Enum t, Num t) => t -> IO Int
adderS n = fmap sum $ sequence [ getL | _ <- [1..n]]
  where getL = do xstr <- getLine
                  return (read xstr :: Int)

{- 6. TODO: Using getCh, define an action readLine :: IO String that behaves in the same way as getLine, except that it
 also permits the delete key to be used to remove characters. Hint: the delete character is '\DEL', and the control
 character for moving the cursor back one space is '\b'. -}

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

-- Backspace deletes the character from the screen but not from the returned value!
getLine' = do x <- getCh
              if x == '\n'
                then do putChar x
                        return []
                else if x == '\DEL'
                     then do putChar '\b'
                             xs <- getLine'
                             return xs
                     else do putChar x
                             xs <- getLine'
                             return (x:xs)
