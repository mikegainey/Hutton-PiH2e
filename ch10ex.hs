-- Graham Hutton: Programming in Haskell
-- Chapter 10 exercises

{- 1. Redifine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ().
-}

putStr' :: String -> IO ()
putStr' xs = sequence_ [ putChar x | x <- xs]

{- 2. Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of any size, rather than
 being specific to boards with just five rows of stars. Hint: first define an auxiliary function that takes the current
 row number as an additional argument. -}

type Board = [Int]

putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

-- I couldn't do this on my own! Had to look it up!
putBoardR' [] r = return ()
putBoardR' (x:xs) r = do putRow r x
                         putBoardR' xs (r+1)

putBoardR b = putBoardR' b 1

{- 3. In a similar manner to the first exercise, redefine the generalized version of putBoard using a list comprehension
 and sequence_. -}

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

adder = do putStr "How many numbers? "
           ns <- getLine
           let n = read ns :: Int
           return n

{- 5. Redefine adder using the function sequence :: [IO a] -> IO [a] that performs a list of actions and returns a list of the resulting values. -}

-- adders = do putStr "How many numbers? "
--             ns <- getLine
--             let n = read ns :: Int
--             xs <- sequence_ [getLine | _ <- [1..n]]
--             putStrLn xs

{- 6. Using getCh, define an action readLine :: IO String that behaves in the same way as getLine, except that it also
 permits the delete key to be used to remove characters. Hint: the delete character is '\DEL', and the control character
 for moving the cursor back one space is '\b'. -}
