-- to see if I can redo this from memory

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

-- list the round winners
firsts :: [[a]] -> [a]
firsts = map head

-- count the occurances of an element in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- remove duplicates of all elements from a list
rmDup :: Eq a => [a] -> [a]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (/=x) xs)

-- remove empty lists from a list of lists
rmNull :: Eq a => [[a]] -> [[a]]
rmNull xss = filter (/=[]) xss

-- results of one elimination round --> [(votes, candidate), (votes, candidate), ...]
roundResults :: [[String]] -> [(Int, String)]
roundResults bs = sort [ ((count c (firsts bs)), c) | c <- rmDup (firsts bs)]

-- eliminate a round loser from the ballots
eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate c css = rmNull $ map (filter (/=c)) css

-- compute the winner of the election
winner bs = case (roundResults bs) of
  [c] -> snd c
  (c:cs) -> winner (eliminate (snd c) bs)
