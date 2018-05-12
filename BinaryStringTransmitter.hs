-- Graham Hutton: Programming in Haskell
-- Binary String Transmitter from chapter 7, page ##

type Bit = Int

-- Note: Bits are listed from lsb to msb (backward from normal)

-- This makes sense to me
bin2int :: [Bit] -> Int
bin2int bits = sum $ zipWith (*) bits weights
  where weights = map (2^) [0..]

-- This is the books preferred way
bin2int' :: [Bit] -> Int
bin2int' = foldr (\x y -> x + 2*y) 0

-- words for positive integers but not zero
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin x = (x `mod` 2) : int2bin (x `div` 2)

