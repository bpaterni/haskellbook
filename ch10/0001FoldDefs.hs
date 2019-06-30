--foldr :: (a -> b -> b) -> b -> [a] -> b
--foldr f z []     = z
--foldr f z (x:xs) = f x (foldr f z xs)

--foldl :: (b -> a -> b) -> b -> [a] -> b
--foldl f acc []     = acc
--foldl f acc (x:xs) = foldl f (f acc x) xs
