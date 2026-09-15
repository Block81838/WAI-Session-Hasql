{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import GHC.Conc (threadDelay)
import Hasql.Connection.Settings (connectionString)
import Hasql.Pool (Pool, acquire)
import Hasql.Pool.Config
  ( settings,
    size,
    staticConnectionSettings,
  )
import Network.Wai.Session.Hasql
  ( HasqlConnectionType (HasqlPool),
    SessionSetting (SessionSetting),
    hasqlStore,
    purgeExpiredSessions,
  )
import Pqi.Native (adapter)
import Test.Hspec
  ( Spec,
    describe,
    hspec,
    it,
    shouldBe,
    shouldContain,
    shouldNotBe,
  )

main :: IO ()
main = hspec spec

testConnection :: IO Pool
testConnection = acquire adapter (settings [size 10, staticConnectionSettings (connectionString "postgres://dbadmin:P%40ssw0rd@localhost:5432/appdb")])

spec :: Spec
spec = describe "Hasql Store Package Functionality" $ do
  it "Should be able to create new session" $ do
    c <- testConnection
    s <- hasqlStore @IO @T.Text @Aeson.Value (SessionSetting (HasqlPool c) True (60 * 60 * 24) True)

    (_, final) <- s Nothing
    sess_id <- final
    shouldContain (show sess_id) "-"

  it "Should not write empty session to database when ssWriteEmptySession is False" $ do
    c <- testConnection
    s <- hasqlStore @IO @T.Text @Aeson.Value (SessionSetting (HasqlPool c) True (60 * 60 * 24) False)

    (_, final) <- s Nothing
    sess_id <- final
    (_, final2) <- s (Just sess_id)
    sess_id2 <- final2
    shouldNotBe sess_id sess_id2

  it "Should be able to insert and lookup KV pair for new session" $ do
    c <- testConnection
    s <- hasqlStore @IO @T.Text @Aeson.Value (SessionSetting (HasqlPool c) True (60 * 60 * 24) True)

    ((reader, writer), final) <- s Nothing
    writer "test_key" "test_value"
    _ <- final
    mV <- reader "test_key"
    shouldContain (show mV) "test_value"

  it "Should be able to lookup a existing session from incoming cookie" $ do
    c <- testConnection
    s <- hasqlStore @IO @T.Text @Aeson.Value (SessionSetting (HasqlPool c) True (60 * 60 * 24) True)

    ((_, writer), final) <- s Nothing
    sess_id <- final
    writer "test_key_2" "test_value_2"
    _ <- final
    ((reader2, _), final2) <- s (Just sess_id)
    sess_id2 <- final2
    shouldBe sess_id sess_id2
    mV2 <- reader2 "test_key_2"
    shouldContain (show mV2) "test_value_2"

  it "Should be able to update a existing KV in one session" $ do
    c <- testConnection
    s <- hasqlStore @IO @T.Text @Aeson.Value (SessionSetting (HasqlPool c) True (60 * 60 * 24) True)

    ((reader, writer), final) <- s Nothing
    writer "test_key_3" "test_value_3"
    _ <- final
    mV <- reader "test_key_3"
    shouldContain (show mV) "test_value_3"
    writer "test_key_3" "test_value_3 spooky"
    _ <- final
    mV2 <- reader "test_key_3"
    shouldContain (show mV2) "test_value_3 spooky"

  it "The helper function purgeExpiredSessions should be able to purge expired sessions" $ do
    c <- testConnection
    -- Change the expiration time to 10s
    let ss = SessionSetting (HasqlPool c) True 10 True
    s <- hasqlStore @IO @T.Text @Aeson.Value ss

    ((reader, writer), final) <- s Nothing
    writer "test_key_4" "test_value_4"
    sess_id <- final
    mV <- reader "test_key_4"
    shouldContain (show mV) "test_value_4"
    threadDelay 11000000
    purgeExpiredSessions ss
    ((reader2, _), _) <- s (Just sess_id)
    mV2 <- reader2 "test_key_4"
    shouldBe mV2 Nothing
