f :: a -> a -> a -> a; f = undefined

g :: a -> b -> c -> b; g = undefined
--  :t g 0 'c' "woot"
-- g 0 'c' "woot" :: Char

h :: (Num a, Num b) => a -> b -> b; h = undefined
-- :t h 1.0 2
-- h 1.0 2 :: Num b => b
-- 
-- :t h 1 (5.5 :: Double)
-- h 1 (5.5 :: Double) :: Double

jackel :: (Ord a, Eq b) => a -> b -> a ; jackel = undefined
-- :t jackel "keyboard" "has the word jackal in it"
-- jackel "keyboard" "has the word jackal in it" :: [Char]
--
-- :t jackel "keyboard" 
-- jackel "keyboard" :: Eq b => b -> [Char]

kessel :: (Ord a, Num a) => a -> b -> a; kessel = undefined
-- :t kessel 1 2
-- kessel 1 2 :: (Num a, Ord a) => a
--
-- :t kessel 1 (2 :: Integer)
-- kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
--
-- :t kessel  (1 :: Integer) 2
-- kessel  (1 :: Integer) 2 :: Integer


