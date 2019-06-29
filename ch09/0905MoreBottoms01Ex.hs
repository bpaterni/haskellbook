import Data.Bool

x0101 = take 1 $ map (+1) [undefined, 2,3] -- Bottom
x0102 = take 1 $ map (+1) [1, undefined, 3] -- [2]
x0103 = take 2 $ map (+1) [1, undefined, 3] -- Bottom

-- maps a string to a list of bools indicating whether the each
-- character in the string is a vowel or not
itIsMystery :: String -> [Bool]
itIsMystery xs = map (\x -> elem x "aeiou") xs

x0105a = map (^2) [1..10] -- squares of values [1..10]
x0105b = map minimum [[1..10], [10..20], [20..30]] -- [1, 10, 20]
x0105c = map sum [[1..5], [1..5], [1..5]] -- [15,15,15]

x0106 = map (\x -> bool (x) (-x) (x == 3))
