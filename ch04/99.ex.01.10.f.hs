f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f t0 t1 = (
  (
    snd t0,
    snd t1
  ),
  (
    fst t0,
    fst t1
  )
  )
