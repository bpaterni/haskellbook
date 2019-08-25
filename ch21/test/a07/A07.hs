
data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- decoder function that makes an object from String
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the
-- DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- an additional "context initializer",
-- that also has IO
makeIoOnlyObj :: [SomeObj]
              -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query
           -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  --case sequence (map decodeFn a) of
  --  (Left err) -> return $ Left err
  --  (Right res) -> do
  --    a <- makeIoOnlyObj res
  --    return $ Right a
  traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn' :: Query
            -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' =
  ((traverse makeIoOnlyObj
  . mapM decodeFn) =<<) . fetchFn

pipelineFn'' :: Query
             -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' =
  ((traverse makeIoOnlyObj
  . traverse decodeFn) =<<) . fetchFn

main :: IO ()
main = putStrLn "Test suite not yet implemented"
