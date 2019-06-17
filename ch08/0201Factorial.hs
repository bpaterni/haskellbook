fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n-1)
