data Quantum =
  Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)


-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes  = Yes
quantFlip3 No   = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes  = Yes
quantFlip4 No   = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes  = Yes
quantFlip5 No   = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes  = No
quantFlip6 No   = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes  = Both
quantFlip7 No   = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes  = Yes
quantFlip8 No   = No
quantFlip8 Both = No

quantFlip9 :: Quantum -> Quantum
quantFlip9 Yes  = Yes
quantFlip9 No   = No
quantFlip9 Both = Both

quantFlip10 :: Quantum -> Quantum
quantFlip10 Yes  = Yes
quantFlip10 No   = Both
quantFlip10 Both = Both

quantFlip11 :: Quantum -> Quantum
quantFlip11 Yes  = No
quantFlip11 No   = Yes
quantFlip11 Both = No

--convert :: Quantum -> Bool
--convert = undefined

convert1 :: Quantum -> Bool
convert1 Yes  = False
convert1 No   = False
convert1 Both = False

convert2 :: Quantum -> Bool
convert2 Yes  = False
convert2 No   = False
convert2 Both = True

convert3 :: Quantum -> Bool
convert3 Yes  = False
convert3 No   = True
convert3 Both = False

convert4 :: Quantum -> Bool
convert4 Yes  = False
convert4 No   = True
convert4 Both = True

convert5 :: Quantum -> Bool
convert5 Yes  = True
convert5 No   = False
convert5 Both = False

convert6 :: Quantum -> Bool
convert6 Yes  = True
convert6 No   = False
convert6 Both = True

convert7 :: Quantum -> Bool
convert7 Yes  = True
convert7 No   = True
convert7 Both = False

convert8 :: Quantum -> Bool
convert8 Yes  = True
convert8 No   = True
convert8 Both = True

data Quad = One
          | Two
          | Three
          | Four
          deriving (Eq, Show)

--eQuad :: Either Quad Quad
--eQuad = 4 + 4 = 8

--prodQuad :: (Quad, Quad)
--prodQuad = 4 * 4 = 16

--funcQuad :: Quad -> Quad
--funcQuad = 4 ^ 4 = 256

--prodTBool :: (Bool, Bool, Bool)
--prodTBool = 2 * 2 * 2 = 8

--gTwo :: Bool -> Bool -> Bool
--gTwo = (2^2) ^ 2 = 2 ^ (2*2) = 16

--fTwo :: Bool -> Quad -> Quad
--fTwo = (4^4) ^ 2 = 4 ^ (4*2) = 4^8 = 65536
