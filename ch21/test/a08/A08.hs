
import Data.ByteString.Lazy hiding (map)
import Data.Functor.Constant
import Data.Functor.Identity
import Data.Monoid
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

edgeMap f t =
  runIdentity $ traverse (Identity . f) t

xs' = [1,2,3,4,5]
xs = xs' :: [Sum Integer]

foldMap' f t =
  getConstant $ traverse (Constant . f) t

main :: IO ()
main = Prelude.putStrLn "Test suite not yet implemented"
