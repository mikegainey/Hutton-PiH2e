-- Graham Hutton: Programming in Haskell
-- Chapter 7 extended example: voting algorithm: alternate vote

{- In this voting system, each person can vote for as many or as few candidates as they wish, listing them in preference
 order on their ballot (1st choice, 2nd choice, and so on). To decide the winner, any empty ballots are first removed,
 then the candidate with the smallest number of 1st-choice votes is eliminated from the ballots, and the same process is
 repeated until only one candidate remains, who is then declared the winner. -}

import Data.List

ballots :: [[String]]
ballots =  [["Red", "Green"],
            ["Blue"],
            ["Green", "Red", "Blue"],
            ["Blue", "Green", "Red"],
            ["Green"]]

rmempty :: [[a]] -> [[a]]
rmempty = filter (not . null)

elim x = map (filter (/=x))

-- from voting-FPtP
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result vs =sort [((count v vs), v) | v <- (rmdups vs)]
----------

rank :: Ord a => [[a]] -> [a]
rank = map snd . result . map head

winner :: Ord a => [[a]] -> a
winner bs = case rank (rmempty bs) of
  [c]    -> c
  (c:cs) -> winner (elim c bs)
