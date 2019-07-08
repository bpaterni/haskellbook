isJust :: Maybe a -> Bool
isJust (Just x) = True
isJust _        = False

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _       = False

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee z _ Nothing  = z
mayybee z f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe z Nothing  = z
fromMaybe z (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe []    = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes = concat . map (mayybee [] (\x -> [x]))

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe xs
  | any isNothing xs = Nothing
  | otherwise = Just (catMaybes xs)
