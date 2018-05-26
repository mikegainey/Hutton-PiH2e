-- Graham Hutton: Programming in Haskell
-- Hangman, extended example from chapter 10

main = do
  putStr "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it:"
  return $ play word

sgetLine = do x <- getCh
              if x == '\n'
                then do putChar x
                        return []
                else do putChar '-'
                        xs <- sgetLine
                        return (x:xs)

play w = "gainey"


