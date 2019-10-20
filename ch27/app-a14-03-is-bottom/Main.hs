module Main where

-- snd (undefined, 1)
-- No
--
-- let x = undefined
-- let y = x `seq` 1 in snd (x, y)
-- Yes
--
-- length $ [1..5] ++ undefined
-- Yes
--
-- length $ [1..5] ++ [undefined]
-- No, undefined not evaluated
--
-- const 1 undefined
-- No
--
-- const 1 (undefined `seq` 1)
-- No
--
-- const undefined 1
-- Yes

main :: IO ()
main = putStrLn "Entrypoint not yet implemented"
