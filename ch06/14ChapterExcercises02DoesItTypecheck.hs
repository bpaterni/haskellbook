-- x :: Int -> Int
-- x blah = blah + 20
-- 
-- printIt :: IO ()
-- printIt = putStrLn (show x)

-- Did not typecheck due to lack of Show instance
data Person = Person Bool
  deriving Show

printPerson :: Person -> IO ()
printPerson p = putStrLn (show p)

-- Did not typecheck due to lack of Eq instance
data Mood = Blah | Woot
  deriving (Eq, Show)

settleDown x = if x == Woot
                  then Blah
                  else x
-- a) settleDown accepts Blah or Woot
-- b) GHCi will fail since 9 is not a Mood
-- c) GHCi will fail since Mood is not a Ord instance

type Subject = String
type Verb    = String
type Object  = String

data Sentence =
  Sentence Subject Verb Object
  deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks =
  Rocks String
  deriving (Eq, Show)

data Yeah =
  Yeah Bool
  deriving (Eq, Show)

data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show)

-- phew = Papu "chases" True
-- arguments are not the required types
phew = Papu (Rocks "chases") (Yeah True)

truth = Papu
  (Rocks "chomskydoz")
  (Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
-- Papu doesn't implement Ord typeclass
