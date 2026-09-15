# wai-session-hasql

A [wai-session](https://hackage.haskell.org/package/wai-session) store that using PostgreSQL as backend, and [hasql](https://hackage.haskell.org/package/hasql) as db connector.

It supports both single hasql connection and hasql pool connection.

Basic usage, see `example/Main.hs` for a complete example. You will need to replace the connection string with your own PostgreSQL connection string in the example and tests files.

```haskell
main :: IO ()
main = do
  -- Create a vault key
  k <- newKey :: IO (Key (Network.Wai.Session.Session IO T.Text Aeson.Value))
  -- Init Hasql connection pool
  pool <- P.acquire adapter (settings [size 10, staticConnectionSettings (connectionString "postgres://dbadmin:P%40ssw0rd@localhost:5432/appdb")])
  s <- hasqlStore (SessionSetting (HasqlPool pool) True (60 * 60 * 24) False)
  -- Create the wai-session middleware by using withSession function
  let sm = withSession s (B8.pack "hello") defaultSetCookie k
      asess = sm $ app k
  run 3000 asess
```
