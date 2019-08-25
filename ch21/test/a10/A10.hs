
-- Naturality
-- t . traverse f = traverse (t . f)
--
-- t . sequenceA = sequenceA . fmap t
--
--
--
-- Identity
-- traverse Identity = Identity
--
-- sequenceA . fmap Identity = Identity
--
--
--
-- Composition
-- traverse (Compose . fmap g . f) =
--   Compose . fmap (traverse g) . traverse f
--
-- sequenceA . fmap Compose =
--   Compose . fmap sequenceA . sequenceA

main :: IO ()
main = Prelude.putStrLn "Test suite not yet implemented"
