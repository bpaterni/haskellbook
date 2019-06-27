mySqr = [ x^2 | x <- [1..10]]

ex01 = [x | x <- mySqr, rem x 2 == 0]
ex02 = [(x,y) | x <- mySqr, y <- mySqr, x < 50, y > 50]

ex03 = take 5 ex02
