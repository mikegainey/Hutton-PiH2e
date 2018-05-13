-- Graham Hutton: Programming in Haskell
-- Binary String Transmitter from chapter 7, page 82

-- Note: In the book, bits are listed from lsb to msb (backward from normal).
--       In this implementation, bits are listed in their normal order.

import Data.Char

type Bit = Int

-- adds a leading zero to the result (but leading zeroes don't change the value of a number!)
int2bin :: Int -> [Bit]
int2bin 0 = [0]
int2bin n = (int2bin (n `div` 2)) ++ [n `mod` 2]

bin2int :: [Bit] -> Int
bin2int bits = sum $ zipWith (*) bits weights
  where weights = reverse $ take (length bits) $ map (2^) [0..]

-- this works too! (but it's too hard to understand)
bin2int' :: [Bit] -> Int
bin2int' = foldl (\acc b -> acc*2 + b) 0

make8 :: [Bit] -> [Bit]
make8 xs
  | lenxs < 8 = (take (8 - lenxs) (repeat 0)) ++ xs
  | lenxs > 8 = (reverse . take 8 . reverse) xs
  | otherwise = xs
  where lenxs = length xs

-- encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 xs = (take 8 xs) : chop8 (drop 8 xs)

-- decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

channel :: [Bit] -> [Bit]
channel = id

-- transmit :: String -> String
transmit = decode . channel . encode
