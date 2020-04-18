{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}

module Ride.DB where

import Control.Monad.Except (MonadError)
import Data.Pool (Pool, createPool, withResource, destroyAllResources)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationContext (..), MigrationCommand (..), runMigration)
import Ride.App (Config (..))

-- | Pool

acquirePool :: IO (Pool Connection)
acquirePool = createPool (connectPostgreSQL pgUrl) close 1 10 10
  where
    pgUrl = "postgres://postgres:102030@localhost:5432/ride"
  
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

withConn :: (WithDb cfg m) => (Connection -> IO a) -> m a
withConn action = do
  config <- ask
  liftIO $ withResource (getPool config) action

-- withTran :: WithDb Config e m => (Connection -> IO a) -> m a
-- withTran action = withConn $ \conn -> withTransaction conn (action conn)