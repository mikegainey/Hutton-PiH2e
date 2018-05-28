{- 3. In a similar mannor to the first exercise, redefine the generalized version of putBoard using a list comprehension and sequence_. -}

putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard b = sequence_ [putRow row num | (row,num) <- zip [1..] b ]

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

adders = do putStr "How many numbers? "
            ns <- getLine
            let n = read ns :: Int
            xs <- sequence_ [getLine | _ <- [1..n]]
            putStrLn xs
