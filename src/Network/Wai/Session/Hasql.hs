{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Session.Hasql
  ( hasqlStore,
    SessionSetting (..),
    HasqlConnectionType (..),
    HasqlSessionException (..),
    ToSessionKey (..),
    Session (..),
    genNewSession,
    purgeExpiredSessions,
  )
where

import Control.Exception (Exception, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson
  ( FromJSON,
    Result (Error, Success),
    ToJSON (toJSON),
    Value (Object),
    fromJSON,
    object,
  )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Char8 qualified as B8
import Data.Functor.Contravariant ((>$<))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int64)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.TypeID.V7 (genTypeID, getUUID)
import Data.UUID (UUID, fromASCIIBytes, toASCIIBytes)
import Hasql.Connection qualified as C
import Hasql.Decoders qualified as D
import Hasql.Encoders qualified as E
import Hasql.Errors (SessionError)
import Hasql.Pool qualified as P
import Hasql.Session qualified as S
import Hasql.Statement qualified as St
import Network.Wai.Session (SessionStore)

-- | Wrapper for the hasql exceptions when using pool or single connection
data HasqlSessionException = HasqlSessionPoolException P.UsageError | HasqlSessionConnException SessionError deriving (Show)

instance Exception HasqlSessionException

-- | Wrapper for different kinds of Hasql connection type
data HasqlConnectionType = HasqlConnection C.Connection | HasqlPool P.Pool

-- | Settings for Hasql Session Store
data SessionSetting = SessionSetting
  { -- | Hasql connection, pool or single connection.
    ssHasqlConn :: HasqlConnectionType,
    -- | Whether init database table, if True, will init the table with the default schema. @eg. True@
    ssInitDB :: Bool,
    -- | Valid period for a session, unit: seconds. @eg. 60 * 60 * 24@
    ssExpiresAfter :: Int64,
    -- | Whether to store newly generated sessions without any KV data in the database. This means that if a new session is generated without any Key-Value pair inserted, it will not be written to the database to prevent bots or something from trying to spam your website. @eg. False@
    ssWriteEmptySession :: Bool
  }

-- | Haskell representation for 'wai_pg_sessions' table schema
data Session = Session
  { -- | Session ID in UUID(v7) format
    sSessId :: UUID,
    -- | KV store by using JSON
    sData :: Value,
    -- | Timestamp when this session was created
    sCreatedAt :: UTCTime,
    -- | Timestamp when this session was updated
    sUpdatedAt :: UTCTime,
    -- | Timestamp when this session will expire
    sExpiresAt :: UTCTime
  }
  deriving (Show, Eq)

-- | Class for adapting the 'k' parameter in @'Network.Wai.Session.SessionStore' m k v@, this package uses 'Data.Text.Text' as the type of key. If you want to use your own key type with the getter or setter function in 'Network.Wai.Session.SessionStore', you need to implement this class for your own type
class ToSessionKey k where
  toSessionKey :: k -> T.Text

instance ToSessionKey T.Text where
  toSessionKey = id

instance ToSessionKey String where
  toSessionKey = T.pack

-- | A generic Hasql session executor
executeQuery :: HasqlConnectionType -> S.Session a -> IO a
executeQuery (HasqlConnection c) s =
  C.use c s >>= \case
    Left err -> throwIO (HasqlSessionConnException err)
    Right v -> return v
executeQuery (HasqlPool p) s =
  P.use p s >>= \case
    Left err -> throwIO (HasqlSessionPoolException err)
    Right v -> return v

-- | A Hasql decoder to decode one session row to Session data
sessionDecoder :: D.Row Session
sessionDecoder =
  Session
    <$> D.column (D.nonNullable D.uuid)
    <*> D.column (D.nonNullable D.jsonb)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)
    <*> D.column (D.nonNullable D.timestamptz)

-- | A sql statement to init the table
createSessionTableQ :: T.Text
createSessionTableQ =
  "create table if not exists wai_pg_sessions ("
    <> "sess_id uuid primary key,"
    <> "data jsonb default '{}' not null,"
    <> "created_at timestamptz default current_timestamp not null,"
    <> "updated_at timestamptz default current_timestamp not null,"
    <> "expires_at timestamptz default (current_timestamp + interval '30 days') not null"
    <> ")"

-- | A sql statement to query the session row by using sess_id.
selectSessionQ :: St.Statement UUID (Maybe Session)
selectSessionQ = St.preparable sql encoder decoder
  where
    sql = "select * from wai_pg_sessions where sess_id = $1"
    encoder = E.param (E.nonNullable E.uuid)
    decoder = D.rowMaybe sessionDecoder

-- | A sql statement to insert a new session record or update if exists
upsertSessionQ :: St.Statement (Session, Int64) ()
upsertSessionQ = St.preparable sql encoder decoder
  where
    sql = "insert into wai_pg_sessions (sess_id, data, expires_at) values ($1, $2, current_timestamp + make_interval(secs => $4)) on conflict (sess_id) do update set data = $2, updated_at = $3"
    encoder = (sSessId . fst >$< E.param (E.nonNullable E.uuid)) <> (sData . fst >$< E.param (E.nonNullable E.jsonb)) <> (sUpdatedAt . fst >$< E.param (E.nonNullable E.timestamptz)) <> (snd >$< E.param (E.nonNullable E.int8))
    decoder = D.noResult

-- | A session store that using postgresql as db backend, Hasql as db connector
hasqlStore :: forall m k v. (MonadIO m, FromJSON v, ToJSON v, ToSessionKey k) => SessionSetting -> IO (SessionStore m k v)
hasqlStore ss =
  executeQuery (ssHasqlConn ss) (S.script createSessionTableQ) >> return (hasqlStore' ss)

-- | Backend for Hasql session store.
hasqlStore' :: (MonadIO m, FromJSON v, ToJSON v, ToSessionKey k) => SessionSetting -> SessionStore m k v
hasqlStore' ss k = do
  let mUUID = k >>= fromASCIIBytes
  gotSession <- case mUUID of
    (Just x) -> do
      res <- executeQuery (ssHasqlConn ss) (S.statement x selectSessionQ)
      maybe genNewSession return res
    Nothing -> genNewSession
  ref <- newIORef (gotSession, ssWriteEmptySession ss)
  return ((reader ref, writer ref), final ref ss)

-- | A helper function to convert aeson 'Data.Aeson.Value' to aeson 'Data.Aeson.KeyMap.KeyMap'
valueToKeyMap :: Value -> KM.KeyMap Value
valueToKeyMap (Object km) = km
valueToKeyMap _ = KM.empty

-- | The main getter function in @'Network.Wai.Session.Session' m k v@
reader :: (MonadIO m, FromJSON a, ToSessionKey k) => IORef (Session, Bool) -> k -> m (Maybe a)
reader r k = do
  (sess, _) <- liftIO $ readIORef r
  let aesonKey = K.fromText (toSessionKey k)
      dataKM = valueToKeyMap (sData sess)
  case KM.lookup aesonKey dataKM of
    Nothing -> return Nothing
    Just vv -> case fromJSON vv of
      Success v -> return $ Just v
      Error _ -> return Nothing

-- | The main setter function in @'Network.Wai.Session.Session' m k v@
writer :: (MonadIO m, ToJSON a, ToSessionKey k) => IORef (Session, Bool) -> k -> a -> m ()
writer r k v = do
  (sess, _) <- liftIO $ readIORef r
  currentTime <- liftIO getCurrentTime
  let aesonKey = K.fromText (toSessionKey k)
      dataKM = valueToKeyMap (sData sess)
      newData = Object (KM.insert aesonKey (toJSON v) dataKM)
  let newSession = sess {sData = newData, sUpdatedAt = currentTime}
  liftIO $ writeIORef r (newSession, True)

final :: IORef (Session, Bool) -> SessionSetting -> IO B8.ByteString
final r ss = do
  (sess, isModified) <- readIORef r
  case isModified of
    True -> do
      executeQuery (ssHasqlConn ss) (S.statement (sess, ssExpiresAfter ss) upsertSessionQ)
      return $ toASCIIBytes (sSessId sess)
    False -> return $ toASCIIBytes (sSessId sess)

-- | A helper function used to generate a new Session, using "Data.TypeID.V7" as UUIDv7 generator
genNewSession :: IO Session
genNewSession = genTypeID "sess" >>= (\u -> getCurrentTime >>= \now -> return (Session u (object []) now now now)) . getUUID

-- | A helper function used to purge all expired sessions
purgeExpiredSessions :: SessionSetting -> IO ()
purgeExpiredSessions ss = do
  executeQuery (ssHasqlConn ss) (S.script sql)
  where
    sql :: T.Text
    sql = "delete from wai_pg_sessions where expires_at < now()"
