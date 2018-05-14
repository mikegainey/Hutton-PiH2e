-- Graham Hutton: Programming in Haskell
-- Binary String Transmitter with parity bit, chapter 7, exercises 7 and 8

-- Note: In the book, bits are listed from lsb to msb (backward from normal).
--       In this implementation, bits are listed in their normal order.

{- 7. Modify the binary string transmitter example to detect simple transmission errors using the concept of parity bits.
 That is, each eight-bit binary number produced during encoding is extended with a parity bit, set to one if the number
 contains an odd number of ones, and to zero otherwise. In turn, each resulting nine-bit binary number consumed during
 decoding is checked to ensure that its parity bit is correct, with the parity bit being discarded if this is the case,
 and a parity error being reported otherwise.

Hint: the library function error :: String -> a displays the given string as an error message and terminates the
program; the polymorphic result type ensures that error can be used in any context.

8. Test your new string transmitter program from the previous exercise using a faulty communication channel that forgets
the first bit, which can be modeled using the tail function on lists of bits.  -}

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

-- Given a list of 8 bits, add a parity bit (odd = 1) at the msb position
-- addParity :: [Bit] -> [Bit]
addParity bits = parity : bits
  where parity = (`mod` 2) $ sum bits

-- encode :: String -> [Bit]
encode = concat . map (addParity . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 xs = (take 9 xs) : chop9 (drop 9 xs)

-- Given a list of 9 bits, return the 8 data bits if the parity is ok, otherwise error
checkParity :: [Bit] -> [Bit]
checkParity bits
  | pbit == parity = tail bits
  | otherwise      = error "Parity check failed"
  where pbit   = head bits
        parity = (`mod` 2) $ sum $ tail bits

-- decode :: [Bit] -> String
decode = map (chr . bin2int . checkParity) . chop9

channel :: [Bit] -> [Bit]
channel = id

badChannel :: [Bit] -> [Bit]
badChannel = tail

transmit :: String -> String
transmit = decode . channel . encode

badTransmit :: String -> String
badTransmit = decode . badChannel . encode
