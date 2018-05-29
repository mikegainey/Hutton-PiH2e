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

