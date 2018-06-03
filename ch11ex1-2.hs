-- Graham Hutton: Programming in Haskell
-- Chapter 10 exercises (mostly modifiying tic-tac-toe)

import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

{- 1. Using the function gametree, verify that there are 549,946 nodes in the complete game tree for a 3 x 3 tic-tac-toe
 game starting from the empty grid, and that the maximum depth of this tree is 9. -}

-- count the nodes in a tree
countNodes :: Tree a -> Int
countNodes (Node _ ts) = 1 + sum (map countNodes ts)
-- *Main> countNodes (gametree empty O)
-- 549946 (verified)

-- compute tree depth
treeDepth :: Tree a -> Int
treeDepth (Node _ []) = 1
treeDepth (Node _ ts) = 1 + maximum (map treeDepth ts)
-- *Main> treeDepth (gametree empty O)
-- 10 (+1 for the root node)

{- 2. Our tic-tac-toe program always chooses the first move from the list of best moves. Modify the final program to
 choose a random move from the list of best moves, using the function randomRIO :: (Int,Int) -> IO Int from
 System.Random to generate a random integer in the given range.

from play':
...
  | p == X    = do putStr "Player X is thinking... "
                   let bms = bestmoves g p
                   r <- randomRIO (0, ((length bms)-1))
                   (play $! bms !! r) (next p)

new function bestmoves (removed head from bestmove):

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p =[g' | Node (g', p') _ <- ts, p' == best]
  where tree = prune depth (gametree g p)
        Node (_,best) ts = minimax tree
-}



size :: Int
size = 3

type Grid = [[Player]]

data Player = O | B | X deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B -- not used; just for completeness
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full g = all (/=B) (concat g)

turn :: Grid -> Player
turn g = if os <= xs then O else X
  where ps = concat g
        os = length (filter (==O) ps)
        xs = length (filter (==X) ps)

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags)
  where line = all (==p)
        rows = g
        cols = transpose g
        diags = [diag g, diag (map reverse g)]

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..(size-1)]]

won :: Grid -> Bool
won g = wins O g || wins X g

g1 = [[X,O,X], [O,X,O], [O,O,X]]

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where bar = [replicate ((size*4)-1) '-']

myPutGrid :: Grid -> IO ()
myPutGrid g = putStrLn $ unlines $ concat $ intersperse [line] $ map myShowRow g
  where  line = replicate (size*3 + (size-1)) '-'

-- I don't understand beside; myShowRow is much easier for me to understand
showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

-- B should be "   ", not " B "
myShowRow :: [Player] -> [String]
myShowRow players = [above,row,below]
  where row = concat $ intersperse "|" $ map (\x -> " " ++ show x ++ " ") players
        above = concat $ intersperse "|" $ replicate size "   "
        below = above

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs,B:ys) = splitAt i (concat g)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs
                     then return (read xs)
                     else do putStrLn "ERROR: Invalid number"
                             getNat prompt

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

play :: Grid -> Player -> IO ()
play g p = do cls
              goto (1,1)
              putGrid g
              play' g p

play' :: Grid -> Player -> IO ()
play' g p
  | wins O g  = putStrLn "Player O wins!\n"
  | wins X g  = putStrLn "Player X wins!\n"
  | full g    = putStrLn "It's a draw!\n"
  | p == O    = do i <- getNat (prompt p)
                   case move g i p of
                     []   -> do putStrLn "ERROR: Invalid move!"
                                play' g p
                     [g'] -> play g' (next p)
  | p == X    = do putStr "Player X is thinking... "
                   let bms = bestmoves g p
                   r <- randomRIO (0, ((length bms)-1))
                   (play $! bms !! r) (next p)

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

-- human vs computer code follows:

data Tree a = Node a [Tree a] deriving (Show)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p
  | won g = []
  | full g = []
  | otherwise = concat [move g i p | i <- [0..(size^2 - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g  = Node (g, O) []
  | wins X g  = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  where ts' = map minimax ts
        ps = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Player -> Grid
bestmove g p = head [g' | Node (g', p') _ <- ts, p' == best]
  where tree = prune depth (gametree g p)
        Node (_,best) ts = minimax tree

bestmoves :: Grid -> Player -> [Grid]
bestmoves g p =[g' | Node (g', p') _ <- ts, p' == best]
  where tree = prune depth (gametree g p)
        Node (_,best) ts = minimax tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          play empty O
