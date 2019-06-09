chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f = (==) . f

func :: Num b => a -> b
func = undefined

arith :: Num b
  => (a -> b)
  -> Integer
  -> a
  -> b
arith f c = (+(fromIntegral c)) . f
