x2a = concat [[1*6], [2*6], [3*6]]
x2b = "rain" ++ drop 2 "elbow"
x2c = 10 * head [1,2,3]
x2d = (take 3 "Julie") ++ (tail "yes")
x2e = concat [tail [1,2,3],
            tail [4,5,6],
            tail [7,8,9]]

main :: IO ()
main = do
  putStrLn . show $ x2a
  putStrLn . show $ x2b
  putStrLn . show $ x2c
  putStrLn . show $ x2d
  putStrLn . show $ x2e

-- a) "Jules"
-- b) [2,3,5,6,8,9]
-- c) "rainbow"
-- d) [6,12,18]
-- e) 10

-- Correct order:
--  d
--  c
--  e
--  a
--  b
