-- Graham Hutton: Programming in Haskell
-- Nim, extended example from chapter 10

import Data.Char

next 1 = 2
next 2 = 1

type Board = [Int]

initial = [5,4,3,2,1]

finished :: Board -> Bool
finished = all (==0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n| (r, n) <- zip [1..5] board]
  where update r n = if r == row then n - num else n

putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard [a,b,c,d,e] = do putRow 1 a
                          putRow 2 b
                          putRow 3 c
                          putRow 4 d
                          putRow 5 e

getDigit prompt = do putStr prompt
                     x <- getChar
                     newline
                     if isDigit x
                       then do return (digitToInt x)
                       else do putStrLn "ERROR: invalid digit"
                               getDigit prompt

newline = do putStr "\n"

play board player = do newline
                       putBoard board
                       if finished board
                         then do newline
                                 putStr "Player "
                                 putStr (show (next player))
                                 putStrLn " wins!"
                         else do newline
                                 putStr "Player "
                                 putStrLn $ show player
                                 row <- getDigit "Enter a row number: "
                                 num <- getDigit "Stars to remove: "
                                 if valid board row num
                                   then play (move board row num) (next player)
                                   else do newline
                                           putStrLn "ERROR: Invalid move"
                                           play board player
