-- Graham Hutton: Programming in Haskell
-- Game of life, extended example from chapter 10

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p s = do goto p
                 putStr s

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 30

height :: Int
height = 30

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showCells b = sequence_ [ writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbors :: Pos -> [Pos]
neighbors (x,y) = map wrap [(x-1,y),   (x+1,y),
                            (x,y-1),   (x,y+1),
                            (x-1,y-1), (x+1,y-1),
                            (x-1,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = ((x-1) `mod` width + 1, (y-1) `mod` height + 1)

liveNeighbors :: Board -> Pos -> Int
liveNeighbors b = length . filter (isAlive b) . neighbors

survivors :: Board -> [Pos]
survivors b = [ p | p <- b, elem (liveNeighbors b p) [2,3]]

births :: Board -> [Pos]
births b = [ p | p <- rmdups (concat (map neighbors b)),
                 isEmpty b p,
                 liveNeighbors b p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/=x) xs)

oldBirths :: Board -> [Pos]
oldBirths b = [ (x,y) | x <- [1..width],
                     y <- [1..height],
                     isEmpty b (x,y),
                     liveNeighbors b (x,y) == 3]

nextGen :: Board -> Board
nextGen b = survivors b ++ births b

life :: Board -> IO ()
life b = do cls
            showCells b
            wait 50000
            life (nextGen b)

wait n = sequence_ [ return () | _ <- [1..n]]
