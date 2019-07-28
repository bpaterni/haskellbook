replaceWithP = const 'p'

n   = Nothing
w   = Just "woohoo"
ave = Just "Ave"
lms = [ave, n, w]

fmap1 = fmap replaceWithP lms
fmap2 = (fmap . fmap) replaceWithP lms
fmap3 = (fmap . fmap . fmap) replaceWithP lms

ha = Just ["Ha", "Ha"]
lmls :: [Maybe [[Char]]]
lmls = [ha, Nothing, Just []]

-- (fmap . fmap) replaceWithP lmls
-- [Maybe Char]
--
-- (fmap . fmap . fmap) replaceWithP lmls
-- [Maybe [Char]]
--
-- (fmap . fmap . fmap . fmap) replaceWithP lmls
-- [Maybe [[Char]]]

main :: IO ()
main = putStrLn "Test suite not yet implemented"
