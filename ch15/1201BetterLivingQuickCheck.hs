asc :: Eq a
    => (a -> a -> a)
    -> a -> a -> a
    -> Bool
asc (<>) a b c =
  a <> (b <> c) == (a <> b) <> c