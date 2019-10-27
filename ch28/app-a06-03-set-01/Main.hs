module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i+1, v+1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

m2 :: M.Map Int Int
m2 = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (5000, 5000)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

s2 :: S.Set Int
s2 = S.fromList $ take 10000 stream
  where stream = iterate (+1) 5000

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

insertMap :: (Int, Int) -> M.Map Int Int
insertMap (k,v) = M.insert k v m

insertSet :: Int -> S.Set Int
insertSet v = S.insert v s

unionMap :: M.Map Int Int -> M.Map Int Int
unionMap = M.unionWith const m

unionSet :: S.Set Int -> S.Set Int
unionSet = S.union s

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = defaultMain
  --[ bench "member check map" $ whnf membersMap 9999
  --, bench "member check set" $ whnf membersSet 9999
  --]
  --[ bench "member insert map" $ whnf insertMap (10100, 10100)
  --, bench "member insert set" $ whnf insertSet 10100
  --]
  [ bench "member union map" $ whnf unionMap m2
  , bench "member union set" $ whnf unionSet s2
  ]
