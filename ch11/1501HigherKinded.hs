data Silly a b c d =
  MkSilly a b c d deriving Show

data List a = Nil | Cons a (List a)
