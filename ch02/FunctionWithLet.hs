-- FunctionWithLet.hs
module FunctionWithLet where

printInc2 n = let plusTwo = n + 2
              in print plusTwo

-- translation of printInc2 from 'let' to 'lambda'

printInc2' n =
    (\plusTwo -> print plusTwo) $ n + 2
