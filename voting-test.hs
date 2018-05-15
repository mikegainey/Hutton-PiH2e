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

firsts = map head

count x = length . filter (==x)

rmdup [] = []
rmdup (c:cs) = c : rmdup (filter (/=c) cs)

result1 bs = sort [ ((count c (firsts bs)), c) | c <- rmdup (firsts bs)]

eliminate c css = rmNull $ map (filter (/=c)) css

rmNull css = filter (/=[]) css

{-
winner bs = case result1 of
  [c] -> c
  [c:cs] -> winner (elim bs)

  where result1 = result of 1 round
        elim = remove 1 candidate with the fewest 1st choice votes
-}
