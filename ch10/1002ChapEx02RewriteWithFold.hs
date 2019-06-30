myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x acc -> (f x || acc)) False

myElem :: Eq a => a -> [a] -> Bool
myElem c = foldr (\x acc -> (x == c || acc)) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny c = any (==c)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x acc -> f x : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter pred = foldr f []
  where f x acc
          | pred x    = x:acc
          | otherwise = acc

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . myMap f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMaximumBy f = foldr1 ffunc
  where ffunc x lastMax
          | f x lastMax == GT = x
          | otherwise         = lastMax

myMinimumBy :: (a -> a -> Ordering)
            -> [a]
            -> a
myMinimumBy f = foldr1 ffunc
  where ffunc x lastMin
          | f x lastMin == LT = x
          | otherwise         = lastMin
