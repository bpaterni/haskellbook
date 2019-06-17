cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

x0101 = appedCatty "woohoo!"                                -- "woops mrow woohoo!"
x0102 = frappe "1"                                          -- "1 mrow haha"
x0103 = frappe (appedCatty "2")                             -- "woops mrow 2 mrow haha"
x0104 = appedCatty (frappe "blue")                          -- "woops mrow blue mrow haha"
x0105 = cattyConny (frappe "pink") 
                   (cattyConny "green" (appedCatty "blue")) -- "pink mrow haha mrow green mrow woops mrow blue"
x0106 = cattyConny (flippy "Pugs" "are") "awesome"          -- "are mrow Pugs mrow awesome"

sumToN :: (Eq a, Num a) => a -> a
sumToN 0 = 0
sumToN n = (+n) . sumToN $ (n-1)

multiplyBy :: (Integral a) => a -> a -> a
multiplyBy x 0 = 0
multiplyBy x y = (+x) . (multiplyBy x) $ (y-1)

data DividedResult =
  Result Integer Integer
  | DividedByZero
  deriving Show

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go (abs num) (abs denom) 0 (isNegate num denom)
  where go n d count isNeg
          | d == 0 = DividedByZero
          | n < d = Result (doNegate isNeg count) n
          | otherwise =
            go (n-d) d (count+1) isNeg
        isNegate n d
          | n < 0 && d > 0 = True
          | n > 0 && d < 0 = True
          | otherwise      = False
        doNegate isNeg x
          | isNeg == True = negate x
          | otherwise     = x

mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 . mc91 $ (n+11)
