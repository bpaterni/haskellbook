
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type TI = []

main :: IO ()
main = do
  let trigger :: TI (Int, Int, [Int])
      trigger = undefined
  quickBatch $ traversable trigger
