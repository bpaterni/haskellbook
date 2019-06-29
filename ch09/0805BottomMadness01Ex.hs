-- Will it blow up?
x0101 = [x^y | x <- [1..5], y <- [2, undefined]] -- yes
x0102 = take 1 $ x0101 -- no
x0103 = sum [1, undefined, 3] -- yes
x0104 = length [1, 2, undefined] -- no
x0105 = length $ [1,2,3] ++ undefined -- yes
x0106 = take 1 $ filter even [1,2,3,undefined] -- no
x0107 = take 1 $ filter even [1,3,undefined] -- yes
x0108 = take 1 $ filter odd [1,3,undefined] -- no
x0109 = take 2 $ filter odd [1,3,undefined] -- no
x0110 = take 3 $ filter odd [1,3,undefined] -- yes

x0201 = [1,2,3,4,5] -- NF
--x0202 = 1 : 2 : 3 : 4 : _ -- WHNF
x0203 = enumFromTo 1 10 -- neither
x0204 = length [1,2,3,4,5] --neither
x0205 = sum (enumFromTo 1 10) -- neither
x0206 = ['a'..'m'] ++ ['n'..'z'] -- neither
--x0207 = (_, 'b') -- WHNF
