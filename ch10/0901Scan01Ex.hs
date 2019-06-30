fibs = 1 : scanl (+) 1 fibs
-- scanl (+) 1 [1,1,2]
-- 1 : scanl f ((+) 1 1) [1,2]
-- 1 : ((+) 1 1) : scanl f ((+) 

--fibs = take 20 $ 1 : scanl (+) 1 fibs
--fibs = takeWhile (<100) $ 1 : scanl (+) 1 fibs
fibsN = (!!) fibs

factorials = scanl (*) 1 [1..]
factorialsN = (!!) factorials
