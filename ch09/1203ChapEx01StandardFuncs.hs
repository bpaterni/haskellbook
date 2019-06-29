myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
--myElem _ [] = False
--myElem c (x:xs) = x == c || myElem c xs
myElem c = any (==c)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (xs:xss) = xs ++ squish xss

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMaximumBy _ []     = undefined
myMaximumBy f (x:xs) = myMaximumBy' x xs
  where myMaximumBy' lastMax [] = lastMax
        myMaximumBy' lastMax (x:xs) = myMaximumBy' (if f x lastMax == GT then x else lastMax) xs

myMinimumBy :: (a -> a -> Ordering)
            -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy f (x:xs) = myMinimumBy' x xs
  where myMinimumBy' lastMin [] = lastMin
        myMinimumBy' lastMin (x:xs) = myMinimumBy' (if f x lastMin == LT then x else lastMin) xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
