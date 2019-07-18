import Data.Monoid

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
