f :: (a, b) -> (c, d) -> ((b, d), (a, c))
--f = undefined
f (a, b) (c, d) = ((b, d), (a, c))
