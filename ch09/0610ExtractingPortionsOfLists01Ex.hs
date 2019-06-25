myWords :: String -> [String]
myWords [] = []
myWords cs = (takeWhile (/=' ') cs : (myWords $ dropWhile (==' ') (dropWhile (/=' ') cs)))
