{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Lazy.Char8 (pack)
import Data.Text qualified as T
import Data.Vault.Lazy (Key, newKey)
import Data.Vault.Lazy qualified as Vault
import Hasql.Connection.Settings (connectionString)
import Hasql.Pool qualified as P
import Hasql.Pool.Config
  ( settings,
    size,
    staticConnectionSettings,
  )
import Network.HTTP.Types (hContentType, status200)
import Network.Wai
  ( Application,
    Request (pathInfo, vault),
    responseLBS,
  )
import Network.Wai.Handler.Warp (run)
import Network.Wai.Session (Session, withSession)
import Network.Wai.Session.Hasql
  ( HasqlConnectionType (HasqlPool),
    SessionSetting (SessionSetting),
    hasqlStore,
  )
import Pqi.Native (adapter)
import Web.Cookie (defaultSetCookie)

-- When you access any page, it will create session and insert some key-values into this session
app :: Key (Network.Wai.Session.Session IO T.Text Aeson.Value) -> Application
app key req respond = do
  putStrLn "Hello world"
  sessionInsert (T.pack "hello") (Aeson.toJSON insertThis)
  sessionInsert (T.pack "world") "Whoareyou"
  mValue <- sessionLookup (T.pack "hello")
  print mValue
  respond $ responseLBS status200 [(hContentType, B8.pack "text/html")] (pack "<h1>Hello world</h1>")
  where
    insertThis = show $ pathInfo req
    Just (sessionLookup, sessionInsert) = Vault.lookup key (vault req)

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
  putStrLn "Server is running on port 3000"
  run 3000 asess
