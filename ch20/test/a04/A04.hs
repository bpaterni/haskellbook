-- Demonstrating Foldable instances
import Data.Foldable
import Data.Monoid

data Identity a =
  Identity a
  deriving (Eq, Show)

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

main :: IO ()
main = putStrLn "Test suite not yet implemented"
