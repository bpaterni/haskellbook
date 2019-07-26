import Data.Monoid
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m)
               => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityString = Identity String

type IdentityStringAssoc =
  IdentityString -> IdentityString -> IdentityString -> Bool

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x y) <> (Two x' y') = Two (x <> x') (y <> y')

instance ( Semigroup a
         , Monoid a
         , Semigroup b
         , Monoid b
         ) => Monoid (Two a b) where
           mempty = Two mempty mempty


instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b 

type TwoStrStr = Two String String

type TwoStrStrAssoc =  TwoStrStr
                    -> TwoStrStr
                    -> TwoStrStr
                    -> Bool

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c)
  => Semigroup (Three a b c) where
    (Three x y z) <> (Three x' y' z') =
      Three (x <> x') (y <> y') (z <> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
    arbitrary = do
      a <- arbitrary
      b <- arbitrary
      c <- arbitrary
      return $ Three a b c

type ThreeStr3 = Three String String String

type ThreeStr3Assoc =  ThreeStr3
                    -> ThreeStr3
                    -> ThreeStr3
                    -> Bool

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance ( Semigroup a
         , Semigroup b
         , Semigroup c
         , Semigroup d )
         => Semigroup (Four a b c d) where
           (Four w x y z) <> (Four w' x' y' z') =
             Four (w <> w') (x <> x') (y <> y') (z <> z')

instance ( Arbitrary a
         , Arbitrary b
         , Arbitrary c
         , Arbitrary d )
         => Arbitrary (Four a b c d) where
           arbitrary = do
             a <- arbitrary
             b <- arbitrary
             c <- arbitrary
             d <- arbitrary
             return $ Four a b c d

type FourStr4 = Four String String String String
type FourStr4Assoc =  FourStr4
                   -> FourStr4
                   -> FourStr4
                   -> Bool

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary =
    frequency [ (1, return $ BoolConj False)
              , (1, return $ BoolConj True) ]

type BoolConjAssoc =  BoolConj
                   -> BoolConj
                   -> BoolConj
                   -> Bool

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary =
    frequency [ (1, return $ BoolDisj False)
              , (1, return $ BoolDisj True) ]

type BoolDisjAssoc =  BoolDisj
                   -> BoolDisj
                   -> BoolDisj
                   -> Bool

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  xo@(Snd _) <> _ = xo
  _ <> xo@(Snd _) = xo
  _ <> xo@(Fst _) = xo

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [ (1, return $ Fst a)
              , (1, return $ Snd b) ]

type OrStr2 = Or String String
type OrStr2Assoc =  OrStr2
                 -> OrStr2
                 -> OrStr2
                 -> Bool

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

-- TODO: Come back to Combine & Comp at a later time
--genFunc :: (CoArbitrary a, Arbitrary b) => a -> Gen b
--genFunc = do
--  a <- arbitrary
--  return $ coarbitrary a
--
--genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
--genCombine = do
--  f <- genFunc
--  return $ Combine f
--
--instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
--  arbitrary = genCombine
--
--type CombineStrings = Combine String String
--type CombineStringsAssoc =  CombineStrings
--                         -> CombineStrings
--                         -> CombineStrings
--                         -> Bool

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }

instance Semigroup a => Semigroup (Mem s a) where
  (<>) = undefined

instance Monoid a => Monoid (Mem s a) where
  mempty = undefined

f' = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (semigroupAssoc :: TwoStrStrAssoc)
  quickCheck (semigroupAssoc :: ThreeStr3Assoc)
  quickCheck (semigroupAssoc :: FourStr4Assoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrStr2Assoc)
  --quickCheck (semigroupAssoc :: CombineStringsAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidLeftIdentity :: IdentityString -> Bool)
  quickCheck (monoidLeftIdentity :: TwoStrStr -> Bool)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: IdentityString -> Bool)
  quickCheck (monoidRightIdentity :: TwoStrStr -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
