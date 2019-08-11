import Control.Applicative
import Data.Char
import Data.List (elemIndex)
--import Data.Map
import Data.Monoid

--l = lookup 3 [(3, "hello")]
--c (x:xs) = toUpper x : xs
--m = Data.Map.fromList [(3, "hello")]

f x =
  lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai")
           ]

g y =
  lookup y [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha")
           ]

h z =
  lookup z [ (2, 3)
           , (5, 6)
           , (7, 8)
           ]

m x =
  lookup x [ (4, 10)
           , (8, 13)
           , (1, 9001)
           ]

added :: Maybe Integer
added = pure (+3) <*> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1,2,3] [4,5,6]

z :: Maybe Integer
z = lookup 2 $ zip [1,2,3] [4,5,6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

a03x :: Maybe Int
a03x = elemIndex 3 [1..5]

a03y :: Maybe Int
a03y = elemIndex 4 [1..5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> a03x <*> a03y

xs = [1..3]
ys = [4..6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> ((,) <$> x'' <*> y'')

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure x = Identity x
  (Identity f) <*> (Identity x) = Identity (f x)

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a
  => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant x <*> Constant y = Constant (x `mappend` y)

validateLength :: Int
               -> String
               -> Maybe String
validateLength maxLen s =
  if (length s) > maxLen
     then Nothing
      else Just s

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

mkName :: String -> Maybe Name
mkName s =
  Name <$> (validateLength 25) s

mkAddress :: String -> Maybe Address
mkAddress a =
  Address <$> (validateLength 100) a

data Person =
  Person Name Address
  deriving (Eq, Show)

mkPerson :: String
         -> String
         -> Maybe Person
mkPerson n a =
  --case mkName n of
  --  Nothing -> Nothing
  --  Just n' ->
  --    case mkAddress a of
  --      Nothing -> Nothing
  --      Just a' -> 
  --        Just $ Person n' a'
  Person <$> mkName n <*> mkAddress a

xb1 = const <$> Just "Hello" <*> Just "World"
xb2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1,2,3]

main :: IO ()
main = putStrLn "Not implemented!"
