import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- Monad Identity laws
--
-- right identity
-- m        >>= return = m
--
-- left identity
-- return x >>= f      = f x
--
-- ^^^ return should be neutral and not perform any computation
--
--
-- Monad Associativity
--
-- (m >>= f) >>= g  ==  m >>= (\x -> f x >>= g)

data CountMe a =
  CountMe Integer a
  deriving  (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) =
    CountMe i (f a)

instance Applicative CountMe where
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    CountMe (n + n') (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe n' b = f a
    in CountMe (n + n') b

instance Arbitrary a
  => Arbitrary (CountMe a) where
    arbitrary =
      CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

main :: IO ()
main = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
