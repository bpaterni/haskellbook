import Control.Applicative
import Data.Char
import Data.Tuple

boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- fmap boop doop x == (*2) ((+10) x)
bloop :: Integer -> Integer
bloop = fmap boop doop

-- First the fmap
--   (+) . (*2)   :: Num a => a -> a -> a
--   ((+) . (*2)) 5 3
--   13
--   (+) <$> (*2) :: Num a => a -> a -> a
--   ((+) <$> (*2)) 5 3
--   ((+) . (*2)) 5 3
--   ((+) . (*2)) == \x -> (+) (2 * x)
--   ((+) 10) 3
--   13
--
-- ((+) <$> (*2) <*> (+10)) 3
-- (3*2) + (3+10)
-- 6 + 13
-- 19
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop

(<||>) :: (a -> Bool)
       -> (a -> Bool)
       -> a
       -> Bool
(<||>) = liftA2 (||)

cap :: [Char] -> [Char]
cap = map toUpper

rev :: [Char] -> [Char]
rev = reverse

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = cap <$> rev

tupled :: [Char] -> ([Char], [Char])
--tupled = (,) <$> cap <*> rev
--tupled xs = do
--  c <- pure $ cap xs
--  r <- pure $ rev xs
--  (c, r)
tupled xs =
  pure (cap xs) >>=
    (\x -> pure (rev xs) >>=
      (\y -> (x,y)))

tupled' :: [Char] -> ([Char], [Char])
tupled' = swap . tupled

main :: IO ()
main = putStrLn "Test suite not yet implemented"
