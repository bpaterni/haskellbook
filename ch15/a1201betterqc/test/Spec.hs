import Control.Monad
import Data.Monoid
import Test.QuickCheck

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada Nada = Nada
  (<>) Nada fx@(Only _) = fx
  (<>) fx@(Only _) Nada = fx
  (<>) (Only x) (Only y) = Only (x <> y)

instance Monoid a
  => Monoid (Optional a) where
    mempty = Nada

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
  arbitrary =
    frequency [ (1, return Fools)
              , (1, return Twoo) ]

instance Semigroup Bull where
  (<>) _ _ = Fools

instance Monoid Bull where
  mempty = Fools

type BullMappend =
  Bull -> Bull -> Bull -> Bool

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  (<>) (First' Nada) (First' Nada) = First' Nada
  (<>) x (First' Nada) = x
  (<>) (First' Nada) x = x
  (<>) x y = x

instance Monoid (First' a) where
  mempty = First' Nada

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (3, return $ First' (Only a))
              , (1, return $ First' Nada) ]

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId =
  First' String -> Bool

main :: IO ()
--main = putStrLn "Not implemented!"
--main = do
--  let ma = monoidAssoc
--      mli = monoidLeftIdentity
--      mri = monoidRightIdentity
--  quickCheck (ma :: BullMappend)
--  verboseCheck (mli :: Bull -> Bool)
--  quickCheck (mri :: Bull -> Bool)
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
