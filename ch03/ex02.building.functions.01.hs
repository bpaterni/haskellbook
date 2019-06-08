-- Given:  "Curry is awesome"
-- Return: "Curry is awesome!"

xa s = s ++ "!"

-- Given:  "Curry is awesome!"
-- Return: "y"
xb s = s !! 4

-- Given:  "Curry is awesome!"
-- Return: "awesome!"
xc s = drop 9 s
