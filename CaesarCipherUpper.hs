-- chapter 5, exercise 10: modify Caesar Cipher to handle upper-case characters
-- and this is a reimplementation from memory (attempt)
import Data.Char -- for ord and chr

-- lowerString

-- TODO: count needs a string already converted to lowercase
count character string = length $ filter (== character) string

letter2asc :: Char -> Int
letter2asc letter = ord letter

asc2letter :: Int -> Char
asc2letter asc = chr asc

encodeLetter :: Int -> Char -> Char
encodeLetter shift letter
  | isUpper letter = chr ((((ord letter - 65) + shift) `mod` 26) + 65)
  | isLower letter = chr ((((ord letter - 97) + shift) `mod` 26) + 97)
  | otherwise = letter

-- encode: maps shiftLetter
encode shift string = map (encodeLetter shift) string

-- freqs
-- freqs string = map (\c -> 1) lowerString
--   where lowerString = map toLower string
--         lenString = length string

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e)^2)/e | (o,e) <- zip os es ]

-- crack
