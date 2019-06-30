import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

filterDbDate :: [DatabaseItem]
             -> [UTCTime]
filterDbDate = foldr f []
  where
    f (DbDate x) xs = x:xs
    f _          xs = xs  

filterDbNumber :: [DatabaseItem]
               -> [Integer]
filterDbNumber = foldr f []
  where
    f (DbNumber x) xs = x:xs
    f _            xs = xs

mostRecent :: [DatabaseItem]
           -> UTCTime
mostRecent = foldr f (UTCTime 
                      (fromGregorian 0 1 1)
                      (secondsToDiffTime 0))
  where
    f (DbDate x) latest
      | x > latest = x
      | otherwise  = latest
    f _ latest = latest

sumDb :: [DatabaseItem]
      -> Integer
sumDb = foldr f 0
  where
    f (DbNumber x) acc = acc + x
    f _            acc = acc

avgDb :: [DatabaseItem]
      -> Double
avgDb xs = (fromIntegral $ sum dbNums) / (fromIntegral (length dbNums))
  where dbNums = filterDbNumber xs
        
