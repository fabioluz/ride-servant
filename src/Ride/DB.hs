{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}

module Ride.DB
( acquirePool
, destroyPool
, migrateDb
, withConn
, HasPool
, WithDb
) where

import Data.Pool (Pool, createPool, withResource, destroyAllResources)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationContext (..), MigrationCommand (..), runMigration)
import Ride.App (Config (..), lookupSetting)

-- | Pool

getConnString :: IO ByteString
getConnString = lookupSetting "PG_CONN_STR" defConnStr
  where
    defConnStr = "postgres://postgres:102030@localhost:5432/ride"

acquirePool :: IO (Pool Connection)
acquirePool = do
  connStr <- getConnString
  createPool (connectPostgreSQL connStr) close 1 10 10
  
destroyPool :: Pool Connection -> IO ()
destroyPool = destroyAllResources

migrateDb :: Pool Connection -> IO ()
migrateDb pool = withResource pool migrate
  where
    migrate conn = void $ withTransaction conn $ runMigration (ctx conn)
    ctx = MigrationContext cmd False
    cmd = MigrationCommands [ MigrationInitialization
                            , MigrationDirectory "migrations"
                            ]

class HasPool a where
  getPool :: a -> Pool Connection

instance HasPool Config where
  getPool = configPool

type WithDb cfg m = (MonadIO m, MonadReader cfg m, HasPool cfg)

withConn :: WithDb cfg m => (Connection -> IO a) -> m a
withConn action = do
  pool <- asks getPool
  liftIO $ withResource pool action

-- withTran :: WithDb Config e m => (Connection -> IO a) -> m a
-- withTran action = withConn $ \conn -> withTransaction conn (action conn)