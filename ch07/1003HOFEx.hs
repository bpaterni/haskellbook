dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

x01 = dodgy 1 0 -- 1
x02 = dodgy 1 1 -- 11
x03 = dodgy 2 2 -- 22
x04 = dodgy 1 2 -- 21
x05 = dodgy 2 1 -- 12
x06 = oneIsOne 1 -- 11
x07 = oneIsOne 2 -- 21
x08 = oneIsTwo 1 -- 21
x09 = oneIsTwo 2 -- 22
x10 = oneIsOne 3 -- 31
x11 = oneIsTwo 3 -- 23
