module ReplaceExperiment where

replaceWithP :: b -> Char
replaceWithP = const 'p'

n   = Nothing
w   = Just "woohoo"
ave = Just "Ave"
lms :: [Maybe [Char]]
lms = [ave, n, w]

replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f)
            => f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

twiceLifted' :: [Maybe [Char]]
             -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted :: (Functor f2, Functor f1, Functor f)
             => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted =
  (fmap . fmap . fmap) replaceWithP

thriceLifted' :: [Maybe [Char]]
              -> [Maybe [Char]]
thriceLifted' = thriceLifted