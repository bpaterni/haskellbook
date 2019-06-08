-- Type constructor: Mood
-- possible values:  Blah, Woot
data Mood = Blah | Woot
  deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood    _ = Blah
