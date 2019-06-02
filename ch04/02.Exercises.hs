-- not True && true
x0 = not True && True

-- not (x = 6)
x1 = x /= 6
  where x = 5

-- (1 * 2) > 5
x2 = (1 * 2) > 5

-- [Merry] > [Happy]
x3 = "Merry" > "Happy"

-- [1,2,3] ++ "look at me!"
x4 = "1,2,3 " ++ "look at me!"
