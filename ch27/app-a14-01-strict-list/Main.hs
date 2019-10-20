{-# LANGUAGE Strict #-}

module Main where

data List a =
    Nil
  | Cons ~a ~(List a)
  deriving (Show)

--take' :: Int -> List Integer
take' n _
  | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x ~xs) = Cons x (take' (n-1) xs)

map' _ Nil = Nil
map' f (Cons x xs) = Cons (f x) (map' f xs)

repeat' x = xs
  where xs = Cons x xs

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = do
  print $ (take' 10 $ map' (+1) (repeat' 1) :: List Integer)
