-- Graham Hutton: Programming in Haskell
-- Chapter 7 extended example: voting algorithm: first past the post

import Data.List

votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red"]

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- my implementation
myResults [] = []
myResults ballots = candidate : myResults (filter (/= head ballots) ballots)
  where candidate = (count (head ballots) ballots, head ballots)

-- my implementation
myWinner votes = snd $ last $ sort (myResults votes)

-- my implementation
rmdups' [] = []
rmdups' (x:xs) = x : rmdups' (filter (/=x) xs)

-- the book answer
rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)

result vs =sort [((count v vs), v) | v <- (rmdups vs)]

winner :: Ord a => [a] -> a
winner = snd . last . result
