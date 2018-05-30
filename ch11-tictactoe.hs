-- Graham Hutton: Programming in Haskell
-- tic-tac-toe, extended example from chapter 11

import Data.Char
import Data.List
import System.IO

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

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

myShowRow :: [Player] -> [String]
myShowRow r = [blank,row,blank]
  where row = concat $ intersperse "|" $ map (\x -> " " ++ show x ++ " ") r
        blank = concat $ intersperse "|" $ replicate size "   "
 
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
                   if xs /= []
