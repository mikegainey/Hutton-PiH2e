-- playing with data type declarations

type Nat = Int

natFac :: Nat -> Nat
natFac n = product[1..n]

type Pos = (Int, Int)
type Trans = Pos -> Pos
type Distance = Float

over1 :: Trans
over1 (x,y) = (x+1,y)

distance :: Pos -> Pos -> Distance
distance (x1,y1) (x2,y2) = (sqrt . fromIntegral) $ (x2-x1)^2 + (y2-y1)^2

type Pair a = (a,a)

type Assoc k v = [(k,v)]

bq :: Assoc String String
bq = [("Mike", "trumpet"), ("James", "trumpet"), ("Andy", "French horn"), ("Daniel", "trombone"), ("Ben", "tuba")]

find :: Eq k => k -> Assoc k v -> [v]
find k t = [ v | (k',v) <- t, k' == k]

