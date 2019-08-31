-- Chapter Exercises

import Control.Applicative
import Data.Maybe

x = [1,2,3]
y = [4,5,6]
z = [7,8,9]

--lookup :: Eq a => a -> [(a, b)] -> Maybe b
--lookup _ [] = Nothing
--lookup key ((k,v):kvs)
--  | k == key = Just v
--  | otherwise = lookup key kvs

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- zip x and z using variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

-- derive from xs and ys
x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

-- derive from ys and zs
x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

-- derive from input + two applications of z'
x3 :: Integer
   -> (Maybe Integer, Maybe Integer)
x3 a = (z' a, z' a)

-- uncurry :: (a -> b -> c) -> (a, b) -> c

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

-- fromMaybe :: a -> Maybe a -> a

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys)

main :: IO ()
main = do
  print $
    sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> ys)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  putStrLn "-- solutions --"
  print $ and . sequA $ 2
  print $ sequA . fromMaybe 0 $ s'
  print $ bolt . fromMaybe 0 $ ys
