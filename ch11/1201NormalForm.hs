--data Fiction = Fiction deriving Show
--data Nonfiction = Nonfiction deriving Show
--
--data BookType = FictionBook Fiction
--              | NonFictionBook Nonfiction
--              deriving Show

type AuthorName = String

--data Author = Author (AuthorName, BookType)

data Author =
  Fiction AuthorName
  | Nonfiction AuthorName
  deriving (Eq, Show)
