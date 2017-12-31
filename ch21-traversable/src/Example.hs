module Example where

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

-- decoder, make some object from string
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- query, runs against the DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- context initializer with IO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

-- before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  case sequence (map decodeFn a) of
    (Left err) -> return $ Left $ err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- step 1
pipelineFn1 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn1 query = do
  a <- fetchFn query
  case traverse decodeFn a of
    (Left err) -> return $ Left err
    (Right res) -> do
      a <- makeIoOnlyObj res
      return $ Right a

-- step 2
pipelineFn2 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn2 query = do
  a <- fetchFn query
  sequence $ fmap makeIoOnlyObj (traverse decodeFn a)

-- step 3
pipelineFn3 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn3 query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)

-- pointfree
pipelineFn4 :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn4 =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
