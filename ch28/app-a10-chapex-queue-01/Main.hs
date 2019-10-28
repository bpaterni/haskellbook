module Main where

import Criterion.Main

-- From Okasaki's Purely Functional Data Structures
data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x Queue{ enqueue = enq, dequeue = deq } = Queue (x:enq) deq

-- removes an item
pop :: Queue a -> Maybe (a, Queue a)
pop Queue{ enqueue = [], dequeue = [] }  = Nothing
pop Queue{ enqueue = enq, dequeue = [] } = pop (Queue [] (reverse enq))
pop Queue{ enqueue = enq, dequeue = (x:xs) } = Just (x, Queue enq xs)

last' :: [a] -> Maybe a
last' [x] = Just x
last' (_:xs) = last' xs
last' [] = Nothing

lpush :: a -> [a] -> [a]
lpush = (:)

lpop :: [a] -> Maybe (a, [a])
lpop [] = Nothing
lpop xs = Just (last xs, init xs)

initQueue :: Int -> Queue Int
initQueue n = go n (Queue [] [])
  where go 0 q = q
        go n q = go (n-1) (push n q)

initList :: Int -> [Int]
initList n = go n []
  where go 0 xs = xs
        go n xs = go (n-1) (lpush n xs)

popListN :: Int -> Maybe (Int, [Int])
popListN n = go n (initList 1000)
  where go n xs = go' (n-1) (lpop xs)
        go' 0 res = res
        go' n Nothing = Nothing
        go' n (Just (x, xs)) = go' (n-1) (lpop xs)

popQueueN :: Int -> Maybe (Int, [Int])
popQueueN n = go n (initQueue 1000)
  where go n q = go' (n-1) (pop q)
        go' 0 res     = res
        go' n Nothing = Nothing
        go' n (Just (_, q)) = go' (n-1) (pop q)

main :: IO ()
--main = putStrLn "Entrypoint not yet implemented"
main = defaultMain
  [ bench "pop list" $ whnf popListN 1000
  , bench "pop queue" $ whnf popQueueN 1000
  ]
