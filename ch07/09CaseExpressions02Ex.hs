functionC x y =
  case result of
    True  -> x
    False -> y
  where result = x > y

ifEvenAdd2 n =
  case comparison of
    True  -> (n + 2)
    False -> n
  where comparison = even n

nums x =
  case comparison of
    LT -> -1
    EQ -> 0
    GT -> 1
  where comparison = compare x 0
