-- Graham Hutton: Programming in Haskell
-- Hangman, extended example from chapter 10

import System.IO

main = do
  putStr "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it:"
  play word

sgetLine = do x <- getCh
              if x == '\n'
                then do putChar x
                        return []
                else do putChar '-'
                        xs <- sgetLine
                        return (x:xs)

getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play word = do putStr "? "
               guess <- getLine
               if guess == word
                 then do putStrLn "You got it!"
                 else do putStrLn (match word guess)
                         play word

match :: String -> String -> String
match guess word = [if x `elem` word then x else '-' | x <- guess]



