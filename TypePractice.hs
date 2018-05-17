-- playing with data type declarations

type Pos = (Int, Int)
type Trans = Pos -> Pos
type Distance = Float

over1 :: Trans
over1 (x,y) = (x+1,y)

distance :: Pos -> Pos -> Distance
distance (x1,y1) (x2,y2) = (sqrt . fromIntegral) $ (x2-x1)^2 + (y2-y1)^2

type Pair a = (a,a)

type Assoc k v = [(k,v)]

bq :: Assoc String String -- I would rather see [(String, String)]
bq = [("Mike", "trumpet"), ("James", "trumpet"), ("Andy", "French horn"), ("Daniel", "trombone"), ("Ben", "tuba")]

find :: Eq k => k -> Assoc k v -> [v]
find k t = [ v | (k',v) <- t, k' == k]

data Move = North | South | East | West
  deriving (Show)

move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y) = (x+1,y)
move West (x,y) = (x-1,y)

moves :: [Move] -> Pos -> Pos
moves [] p = p
moves (m:ms) p = move m (moves ms p)

rev :: Move -> Move
rev North = South
rev South = North
rev East = West
rev West = East

data Shape = Circle Float | Rectangle Float Float
  deriving (Show)

-- create a square
square :: Float -> Shape
square s = Rectangle s s

-- calculate area of circles and squares
area :: Shape -> Float
area (Circle r) = pi*r^2
area (Rectangle x y) = x*y

data Nat = Zero | Succ Nat
  deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

inc :: Nat -> Nat
inc n = Succ n

dec :: Nat -> Nat
dec Zero = Zero
dec (Succ n) = n


