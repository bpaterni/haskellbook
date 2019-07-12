module BasicQuickCheckTest
  where

import BasicQuickCheck
  ( half
  , halfIdentity
  )
import Data.Char (toUpper)
import Data.List (sort)
import Test.QuickCheck

prop_isDoubleHalfEqual :: Double -> Bool
prop_isDoubleHalfEqual x = (==x) . halfIdentity $ x

isListOrdered :: (Ord a) => [a] -> Bool
isListOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t)      = (Just y, t)
        go y (Just x, t)       = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z

plusCommutative x y z =
  x + y == y + x

multAssociative x y z =
  x * (y * z) == (x * y) * z

multCommutative x y z =
  x * y == y * x

nonNegativeZeroIntegralGen :: ( Arbitrary a
                             , Integral a ) => Gen a
nonNegativeZeroIntegralGen = do
  a <- arbitrary
  return (if a /= 0 then abs a else a+1)

nonNegativeZeroIntegralPairGen :: ( Arbitrary a
                                  , Integral a
                                  , Arbitrary b
                                  , Integral b
                                  ) =>
                                    Gen (a, b)
nonNegativeZeroIntegralPairGen = do
  a <- arbitrary
  b <- arbitrary
  return ( if a /= 0 then abs a else a+1
         , if b /= 0 then abs b else b+1 )

intToNonNegZero :: (Integral a) => a -> a
intToNonNegZero x = if x /= 0 then abs x else x+1

nonNegativeZeroIntegralTripleGen :: ( Arbitrary a
                                    , Integral a
                                    , Arbitrary b
                                    , Integral b
                                    , Arbitrary c
                                    , Integral c
                                    ) => Gen (a,b,c)
nonNegativeZeroIntegralTripleGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return ( intToNonNegZero a
         , intToNonNegZero b
         , intToNonNegZero c )

prop_quotRem :: Property
prop_quotRem =
  forAll (nonNegativeZeroIntegralPairGen :: Gen (Integer, Integer))
  (\(x,y) -> (quot x y)*y + (rem x y) == x)

prop_divMod :: Property
prop_divMod =
  forAll (nonNegativeZeroIntegralPairGen :: Gen (Integer, Integer))
  (\(x,y) -> (div x y)*y + (mod x y) == x)

expAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z

prop_expAssociative :: Property
prop_expAssociative =
  forAll (nonNegativeZeroIntegralTripleGen :: Gen (Integer, Integer, Integer))
  (\(x,y,z) -> x ^ (y ^ z) == (x ^ y) ^ z)

prop_expCommutative :: Property
prop_expCommutative =
  forAll (nonNegativeZeroIntegralPairGen :: Gen (Integer, Integer))
  (\(x,y) -> x ^ y == y ^ x)

listDoubleReversedEqual xs = (reverse . reverse $ xs) == id xs

nonNegativeZeroIntegralListPairGen :: ( Arbitrary a
                                      , Integral a
                                      , Arbitrary b
                                      ) =>
                                        Gen (a, [b])
nonNegativeZeroIntegralListPairGen = do
  a <- arbitrary
  b <- arbitrary
  return ( if a /= 0 then abs a else a+1
         , [b] )

prop_lenTakeN :: Property
prop_lenTakeN =
  forAll (nonNegativeZeroIntegralListPairGen :: Gen (Integer, [Integer]))
  (\(n,xs) -> fromIntegral (length (take (fromIntegral n) xs)) == n)

square x = x * x

squareIdentity (NNDouble x) = NNDouble (square (sqrt x))

data Nat a = Nat a deriving (Eq, Show)

integerToNat :: Integral a => a -> Nat a
integerToNat i
  | i < 0     = Nat (negate i)
  | otherwise = Nat i

natGen :: (Arbitrary a, Integral a) => Gen (Nat a)
natGen = do
  a <- arbitrary
  return (integerToNat a)

instance (Arbitrary a, Integral a) => Arbitrary (Nat a) where
  arbitrary = natGen

newtype NNDouble =
  NNDouble Double deriving (Eq, Ord, Show)

doubleToNNDouble :: Double -> NNDouble
doubleToNNDouble f
  | f < 0     = NNDouble (negate f)
  | otherwise = NNDouble f

nnDoubleGen :: Gen NNDouble
nnDoubleGen = do
  a <- arbitrary
  return (doubleToNNDouble a)

instance Arbitrary NNDouble where
  arbitrary = nnDoubleGen

twice f = f . f
fourTimes = twice . twice

alphaLowerUpper = concat [['a'..'z'],['A'..'Z']]

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)
  | elem x alphaLowerUpper = toUpper x : xs
  | otherwise = x : capitalizeWord xs

data Fool = Fulse | Frue deriving (Eq, Show)

foolGenEqual :: Gen Fool
foolGenEqual = do
  oneof [ return $ Fulse
        , return $ Frue ]

foolGenMoreFulse :: Gen Fool
foolGenMoreFulse = do
  frequency [ (1, return Frue)
            , (2, return Fulse) ]

instance Arbitrary Fool where
  --arbitrary = foolGenEqual
  arbitrary = foolGenMoreFulse

runQc :: IO ()
runQc = do
  quickCheck prop_isDoubleHalfEqual
  quickCheck ((\xs -> isListOrdered . sort $ xs) :: [Integer] -> Bool)
  quickCheck (plusAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multAssociative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (plusCommutative :: Integer -> Integer -> Integer -> Bool)
  quickCheck (multCommutative :: Integer -> Integer -> Integer -> Bool)
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_expAssociative
  quickCheck prop_expCommutative
  quickCheck (listDoubleReversedEqual :: [Integer] -> Bool)
  quickCheck ((\x -> ((+1) $ x) == (+1) x) :: Integer -> Bool)
  quickCheck ((\x -> (((+1) . (+2)) x) == ((+1) ((+2) x))) :: Integer -> Bool)
  quickCheck ((\xs ys -> foldr (:) xs ys == (++) xs ys) :: [Integer] -> [Integer] -> Bool)
  quickCheck ((\xs -> foldr (++) [] xs == concat xs) :: [[Integer]] -> Bool)
  quickCheck prop_lenTakeN
  quickCheck ((\x -> read (show x) == x) :: Integer -> Bool)
  quickCheck ((\x -> squareIdentity x == x) :: NNDouble -> Bool) -- Floats/Doubles lose precision as calculations are performed
  quickCheck ((\x -> (capitalizeWord x == twice capitalizeWord x)
                     &&
                     (capitalizeWord x == fourTimes capitalizeWord x)) :: String -> Bool)
  quickCheck ((\x -> (sort x == twice sort x) && sort x == fourTimes sort x) :: [Integer] -> Bool)
  quickCheck ((\x -> x == x) :: Fool -> Bool)

main :: IO ()
main = runQc
