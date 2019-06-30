x0101 = foldr (*) 1 [1..5]
-- Same as
-- foldl (flip (*)) 1 [1..5]
-- foldl (*) 1 [1..5]

x0102 = foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) 1 [1..3]
-- foldl (flip (*)) (f 1 1) [2..3]
-- foldl f          (f (f 1 1) 2) [3]
-- foldl f          (f (f (f 1 1) 2) 3) []
-- (f (f (f 1 1) 2 ) 3)
-- (f (f (1) 2) 3)
-- (f (2) 3)
-- (6)

x0105a = foldr (++) [] ["woot", "WOOT", "woot"]
x0105b = foldr max ' ' "fear is the little death"
x0105c = foldr (&&) True [False, True]
x0105d = foldr (||) True [False, False]
x0105e = foldl (flip ((++) . show)) "" [1..5]
-- foldl ((++) . show) "" [1..5]
-- foldl ((++) . show) (f "" 1) [2..3]
-- foldl f             (f (f "" 1) 2) [3]
-- foldl f             (f (f (f "" 1) 2) 3) []
-- (f (f (f "" 1) 2) 3)
x0105f = foldr const 'a' ['1'..'5']
x0105g = foldr const 'a' "tacos"
x0105h = foldl (flip const) 'a' "burritos"
x0105i = foldl (flip const) 'z' ['1'..'5']
