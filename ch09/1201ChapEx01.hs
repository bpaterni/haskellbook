import Data.Char

x02RmUpper = filter $ isUpper
x03CapSentence [] = []
x03CapSentence (x:xs) = toUpper x : xs

x04Yell [] = []
x04Yell (x:xs) = toUpper x : x04Yell xs

x05CapHead = toUpper . head
