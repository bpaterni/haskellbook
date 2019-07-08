lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where f (Left x) acc = x : acc
        f _ acc = acc

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where f (Right x) acc = x : acc
        f _ acc = acc

partitionEithers' :: [Either a b]
                  -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where f (Right r) (ls, rs) = (ls, r:rs)
        f (Left l)  (ls, rs) = (l:ls, rs)

eitherMaybe' :: (b -> c)
             -> Either a b
             -> Maybe c
eitherMaybe' _ (Left x) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c)
        -> (b -> c)
        -> Either a b
        -> c
either' f _ (Left x)  = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c)
              -> Either a b
              -> Maybe c
eitherMaybe'' f = either' (\x -> Nothing) (\x -> Just (f x))
