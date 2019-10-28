module Main where

import Criterion.Main
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

v :: V.Vector Int
v = V.fromList [1..1000]

uv :: UV.Vector Int
uv = UV.fromList [1..1000]

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = defaultMain
  [ bench "slicing vector" $ whnf (V.head . V.slice 100 900) v
  , bench "slicing unboxed vector" $ whnf (UV.head . UV.slice 100 900) uv
  ]
