stops  = "pbtdkg"
vowels = "aeiou"

fst3Tup (x,_,_) = x

x01a = [(x,y,z) | x <- stops, y <- vowels, z <- stops]
x01b = filter ((=='p') . fst3Tup) x01a

nouns = ["people", "history", "way", "art", "world"]
verbs = ["beat", "become", "begin", "bend"]

x01c = [(x,y,z) | x <- nouns, y <- verbs, z <- nouns]
x01c' = filter ((=="people") . fst3Tup) x01c

-- Average number of letters in a string composed of words
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' :: String -> Double
seekritFunc' x =
  (/) (fromIntegral (sum (map length (words x))))
      (fromIntegral (length (words x)))
