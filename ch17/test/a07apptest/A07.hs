import Data.Functor
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Applicative Laws

-- Identity
-- pure id <*> v = v
--
-- Composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
--
-- Homomorphism
-- pure f <*> pure x = pure (f x)
--
-- Interchange
-- u <*> pure y = pure ($ y) <*> u

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo)
              ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools
  mappend _ _ = Fools

-- from the checkers library
instance EqProp Bull where
  (=-=) = eq

xs = [("b", "w", (1 :: Int))]

type SSI = (String, String, Int)

trigger :: [SSI]
trigger = undefined

main :: IO ()
main = do
  quickBatch (monoid Twoo)
  --quickBatch $ applicative xs
  quickBatch (applicative trigger)
