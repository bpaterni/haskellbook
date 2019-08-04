import Test.QuickCheck

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Show)

instance Functor (Constant m) where
  fmap _ (Constant v) = Constant v

--fc = fmap (const 3)
--fc' = fmap (const 5)
--separate = fc . fc'
--c = const 3
--c' = const 5
--fused = fmap (c . c')
--cw = Constant "WOOHOO"
