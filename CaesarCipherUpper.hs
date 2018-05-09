-- chapter 5, exercise 10: modify Caesar Cipher to handle upper-case characters
-- and this is a reimplementation from memory (and it works!)
import Data.Char -- for ord and chr

letters :: [Char]
letters = ['A'..'Z'] ++ ['a'..'z']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [ b | (a,b) <- zip xs [0..], a == x ]

minpos :: Ord a => [a] -> Int
minpos xs = head $ positions (minimum xs) xs

lowerString :: String -> String
lowerString string = map toLower string

count :: Char -> String -> Int
count character string = length $ filter (== character) (lowerString string)

letter2asc :: Char -> Int
letter2asc letter = ord letter

asc2letter :: Int -> Char
asc2letter asc = chr asc

encodeLetter :: Int -> Char -> Char
encodeLetter shift letter
  | isUpper letter = chr ((((ord letter - 65) + shift) `mod` 26) + 65)
  | isLower letter = chr ((((ord letter - 97) + shift) `mod` 26) + 97)
  | otherwise = letter

encode :: Int -> String -> String
encode shift string = map (encodeLetter shift) string

percent :: Int -> Int -> Float
percent n d = (fromIntegral n) / (fromIntegral d) * 100

freqs :: String -> [Float]
freqs string = map (\c -> percent (count c string) letterCount) ['a'..'z']
  where letterCount = length $ filter (\c -> c `elem` letters) string

normfreq :: [Float]
normfreq = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
            0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
            6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [ ((o - e)^2)/e | (o,e) <- zip os es ]

rotate :: Int -> [a] -> [a]
rotate n xs = (drop n xs) ++ (take n xs)

chitab :: String -> [Float]
chitab string = [ chisqr (rotate r strfreq) normfreq | r <- [0..25] ]
  where strfreq = freqs string

crack :: String -> String
crack string = encode (negate rotation) string
  where rotation = minpos $ chitab string
