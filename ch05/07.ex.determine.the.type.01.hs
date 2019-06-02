{-# LANGUAGE NoMonomorphismRestriction #-}
module DetermineTheType where 

example = 1

x01a = (* 9) 6
x01b = head [(0,"doge"), (1,"kitteh")]
x01c = head [(0 :: Integer, "doge"), (1, "kitteh")]
x01d = if False then True else False
x01e = length [1..5]
x01f = (length [1..4]) > (length "TACOCAT")
