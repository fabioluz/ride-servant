{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Ride.DB where

import Control.Monad.Except (MonadError)
import Data.Pool (Pool, createPool, withResource, destroyAllResources)
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL, close, withTransaction)
import Database.PostgreSQL.Simple.Migration (MigrationContext (..), MigrationCommand (..), runMigration)
import Ride.App (Env (..))

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

instance HasPool Env where
  getPool = envPool

type WithDb env m = (MonadIO m, MonadReader env m, HasPool env)

-- | Utilities

data DbError = DbError String
  deriving (Show, Exception)

withConn :: (WithDb env m) => (Connection -> IO a) -> m a
withConn action = do
  env <- ask
  liftIO $ withResource (getPool env) action

-- withTran :: WithDb env e m => (Connection -> IO a) -> m a
-- withTran action = withConn $ \conn -> withTransaction conn (action conn)