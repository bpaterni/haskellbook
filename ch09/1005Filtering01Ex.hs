x0101 = filter (\x -> (==) 0 $ rem x 3)
x0102 = length . x0101 $ [1..30]
x0103 = filter isNotExcludeWord . words
  where isNotExcludeWord x
          | x == "the" = False
          | x == "a"   = False
          | x == "an"  = False
          | otherwise  = True
