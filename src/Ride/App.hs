{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ride.App
( App
, AppT (..)
, Config (..)
, WithLogger
, logInfo
, Env (..)
, WithEnv
, getEnv
) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

newtype AppT m a = AppT { unApp :: ReaderT Config m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Config
           )

type App = AppT IO

data Config = Config
  { configPort   :: !Int 
  , configEnv    :: !Env
  , configPool   :: !(Pool Connection)
  , configLogger :: !Logger
  }

-- |
-- | Logger
-- |

type Logger = String -> IO ()

class HasLogger a where
  getLogger :: a -> Logger

instance HasLogger Config where
  getLogger = configLogger

type WithLogger cfg m = (MonadIO m, MonadReader cfg m, HasLogger cfg)

logInfo :: (WithLogger cfg m, Show a) => a -> m ()
logInfo msg = do
  config <- ask
  liftIO $ getLogger config $ show msg

-- |
-- | Env
-- |

data Env = Development | Production

class HasEnv a where
  getEnv :: a -> Env

instance HasEnv Config where
  getEnv = configEnv

type WithEnv cfg m = (MonadIO m, MonadReader cfg m, HasEnv cfg)


