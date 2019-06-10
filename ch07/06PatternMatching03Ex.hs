k (x, y) = x
-- k :: (a, b) -> a
k1 = k ((4-1), 10)
k2 = k ("three", (1+2))
-- k2 :: String -- not same as either k1 or k3
k3 = k (3, True)

-- k1 AND k3 return 3

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a, d), (c, f))
