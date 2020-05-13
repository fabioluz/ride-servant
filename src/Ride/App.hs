{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ride.App
( App
, AppT (..)
, Config (..)
, WithLogger
, logInfo
, Env (..)
, WithEnv
, getEnv
, lookupSetting
) where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant.Auth.Server (ThrowAll, throwAll)
import System.Environment (lookupEnv)

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
  logger <- asks getLogger
  liftIO . logger $ show msg

-- |
-- | Env
-- |

data Env = Development | Production
  deriving (Show, Read)

class HasEnv a where
  getEnv :: a -> Env

instance HasEnv Config where
  getEnv = configEnv

type WithEnv cfg m = (MonadIO m, MonadReader cfg m, HasEnv cfg)

-- |
-- | Utils
-- | 

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
  envRes <- lookupEnv env
  pure $ fromMaybe def $ envRes >>= readMay

-- |
-- | Servant Configuration
-- |

instance MonadIO m => ThrowAll (AppT m a) where
  throwAll = throwIO


