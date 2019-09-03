import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

oneEnd :: Parser ()
oneEnd = one >> eof

oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop

oneTwoEnd :: Parser ()
oneTwoEnd = oneTwo >> eof

p123 :: Parser String
p123 = do
  n <- (show <$> integer)
  _ <- eof
  return n

string' :: String -> Parser String
string' str = go str mempty
  where
    go [] parsed     = return parsed
    go (x:xs) parsed = char x >>= (\x' -> go xs (parsed ++ [x']))

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

pNL = putStrLn . ('\n':)

main :: IO ()
main = putStrLn "Test suite not yet implemented"
--main = do
--  pNL "stop:"
--  testParse stop
--
--  pNL "one:"
--  testParse one
--
--  pNL "one':"
--  testParse one'
--
--  pNL "oneTwo:"
--  testParse oneTwo
--
--  pNL "oneTwo':"
--  testParse oneTwo'
