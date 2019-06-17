--f :: Bool -> Int
----f True = error "blah"
--f False = 0

f :: Bool -> Maybe Int
f False = Just 0
f _     = Nothing
