import Control.Monad (ap)

-- ??? how does it loop over the input list
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  --x <- xs
  --if even x
  --   then [x*x, x*x]
  --   else [x*x]
  x <- xs
  if even x
     then [x*x, x*x]
     else []

data Cow = Cow { name :: String
               , age :: Int
               , weight :: Int
               } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
   in if n == "Bess" && w > 499
         then Nothing
         else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String
                -> Int
                -> Int
                -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy   <- noEmpty name'
  agey    <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String
                 -> Int
                 -> Int
                 -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
    \nammy ->
      noNegative age' >>=
        \agey ->
          noNegative weight' >>=
            \weighty ->
              weightCheck (Cow nammy agey weighty)

f :: Integer -> Maybe Integer
f 0 = Nothing
f n = Just n

g :: Integer -> Maybe Integer
g i =
  if even i
     then Just (i + 1)
     else Nothing

h :: Integer -> Maybe String
h i = Just ("10191" ++ show i)

doSomething' n = do
  a <- f n
  b <- g a
  c <- h b
  pure (a, b, c)

type Founded = Int

type Coders = Int

data SoftwareShop =
  Shop { founded :: Founded
       , programmers :: Coders
       } deriving (Eq, Show)

data FoundedError =
    NegativeYears Founded
  | TooManyYears Founded
  | NegativeCoders Coders
  | TooManyCoders Coders
  | TooManyCodersForYears Founded Coders
  deriving (Eq, Show)

validateFounded :: Int -> Either FoundedError Founded
validateFounded n
  | n < 0     = Left $ NegativeYears n
  | n > 500   = Left $ TooManyYears n
  | otherwise = Right n

validateFounded' :: Int -> Sum FoundedError Founded
validateFounded' n
  | n < 0     = First $ NegativeYears n
  | n > 500   = First $ TooManyYears n
  | otherwise = Second n

validateCoders :: Int -> Either FoundedError Coders
validateCoders n
  | n < 0     = Left $ NegativeCoders n
  | n > 5000  = Left $ TooManyCoders n
  | otherwise = Right n

validateCoders' :: Int -> Sum FoundedError Coders
validateCoders' n
  | n < 0     = First $ NegativeCoders n
  | n > 5000  = First $ TooManyCoders n
  | otherwise = Second n

mkSoftware :: Int -> Int -> Either FoundedError SoftwareShop
mkSoftware years coders = do
  founded <- validateFounded years
  programmers <- validateCoders coders

  if programmers > div founded 10
     then Left $
       TooManyCodersForYears founded programmers
     else Right $ Shop founded programmers

mkSoftware' :: Int -> Int -> Sum FoundedError SoftwareShop
mkSoftware' years coders = do
  founded <- validateFounded' years
  programmers <- validateCoders' coders

  if programmers > div founded 10
     then First $
       TooManyCodersForYears founded programmers
     else Second $ Shop founded programmers

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a)  = First a

instance Applicative (Sum a) where
  pure = Second
  Second f <*> Second x = Second (f x)
  First x <*> _ = First x
  _ <*> First x = First x

instance Monad (Sum a) where
  return = pure
  Second x >>= f = f x
  First x >>= _ = First x

main :: IO ()
main = putStrLn "Not implemented!"
