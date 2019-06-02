awesome    = ["Pupuchon", "curry", ":)"]
also       = ["Quake", "The Simons"]
allAwesome = [ awesome, also ]

-- 01. Type signature of `length`?
--   [a] -> Int
--   :t length
--   length :: Foldable t => t a -> Int

-- 03 Fix 6 / length [1,2,3]
x03 = (/) 6 $ fromIntegral $ length [1,2,3]

-- 04 Fix 6 / length [1,2,3] using different operator
x04 = div 6 $ fromIntegral $ length [1,2,3]
