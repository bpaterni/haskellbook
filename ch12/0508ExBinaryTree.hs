data BinaryTree a =
  Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a))
       -> a
       -> BinaryTree b
unfold f x =
  case f x of
    Nothing -> Leaf
    Just (nextl, succ, nextr) ->
      Node (unfold f nextl) succ (unfold f nextr)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f n
  where f x
          | x < 1 = Nothing
          | otherwise = Just (x-1, n-x, x-1)
