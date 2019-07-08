import Data.List

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

f b
  | b == 6 = Nothing
  | otherwise = Just (b, b+1)

testUnfoldr = unfoldr f 0

myUnfoldr :: (b -> Maybe (a,b))
         -> b
         -> [a]
myUnfoldr f x =
  case f x of
    Nothing -> []
    Just (next, succ) -> next : myUnfoldr f succ

betterIterate :: (a -> a) -> a -> [a]
betterIterate f z = myUnfoldr (\x -> Just (x, f x)) z
