{-# LANGUAGE FlexibleInstances #-}
import GHC.Arr

-- Functor Bool -- not possible, kind *

data BoolAndSomethingElse a =
  False' a | True' a
  deriving (Eq, Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' (f x)
  fmap f (True' x)  = True' (f x)

data BoolAndMaybeSomethingElse a =
    Falsish
  | Truish a
  deriving (Eq, Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish (f x)

newtype Mu f = InF { outF :: f (Mu f) }

-- not possible, Mu has kind (* -> *) -> *
--instance Functor Mu where
--  fmap = undefined

data D =
  D (Array Word Word) Int Int

-- not possible, D has kind *
--instance Functor D where
--  fmap = undefined

data Sum b a =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b

data Company a c b =
    DeepBlue a c
  | Something b
  deriving (Eq, Show)

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

data More b a =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

data Quant a b =
    Finance
  | Desk a
  | Bloor b
  deriving (Eq, Show)

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor x) = Bloor (f x)

data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K x) = K x

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
  fmap f (Flip (K' x)) =
    Flip $ K' (f x)

data EvilGoateeConst a b =
  GoatyConst b
  deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst x) = GoatyConst (f x)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f
  => Functor (LiftItOut f) where
    fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

data Parappa f g a =
  DaWrappa (f a) (g a)
  deriving (Eq, Show)

instance (Functor f, Functor g)
  => Functor (Parappa f g) where
    fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)
  deriving (Eq, Show)

instance Functor g
  => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
  deriving (Eq, Show)

instance (Functor g)
  => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x rest) = Cons (f x) (fmap f rest)

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats ( GoatLord a )
              ( GoatLord a )
              ( GoatLord a )
  deriving (Eq, Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats x y z) =
    MoreGoats (fmap f x) (fmap f y) (fmap f z)

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Show a => Show (TalkToMe a) where
  show Halt = "halt"
  show (Print s a) = concat ["Print ", s, " ", show a]
  show (Read g) = "Read"

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read (fmap f g)
