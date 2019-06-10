mTh0 x y z = x * y * z
mTh1 x y   = \z -> x * y * z
mTh2 x     = \y -> \z -> x * y * z
-- 
mTh3       = \x -> \y -> \z -> x * y * z

-- mTh0 3 :: Num a => a -> a -> a

addOneIfOdd n = case odd n of
                  True -> f n
                  False -> n
                  where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x
