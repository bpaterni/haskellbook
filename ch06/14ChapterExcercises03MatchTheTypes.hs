import Data.List (sort)

i :: Num a => a
-- i :: a -- Does not typecheck, no Num instance
i = 1

--f :: Float
--f :: Num a => a -- Does not typecheck, literal is Fractional
--f :: Fractional a => a -- OK!
f :: RealFrac a => a -- OK!
f = 1.0

--freud :: a -> a
freud :: Ord a => a -> a -- OK!
freud x = x

--freud' :: a -> a
freud' :: Int -> Int -- OK!
freud' x = x

myX = 1 :: Int

sigmund :: Int -> Int
--sigmund :: a -> a -- Does not typecheck, Int is not general enough for a -> a
sigmund x = myX

sigmund' :: Int -> Int
--sigmund' :: Num a => a -> a -- Does not typecheck, Int is not general enough for a -> a
sigmund' x = myX

--jung :: Ord a => [a] -> a
jung :: [Int] -> Int -- OK!
jung xs = head (sort xs)

--young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs)

mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
--signifier :: Ord a => [a] -> a -- Does not typecheck, mySort is not general enough for [a] -> a
signifier xs = head (mySort xs)
