bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) prevBools ++ map (True:) prevBools
  where prevBools = bools (n-1)
