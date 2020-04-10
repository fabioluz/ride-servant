{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Ride.App where

import Control.Monad.Except (MonadError (..))
import Data.Aeson (encode)
import Data.List.NonEmpty (NonEmpty)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant (ServerError, err500, err401, err422, errBody)

newtype AppT m a = AppT { unApp :: ReaderT Env m a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Env
           )

type App a = AppT IO a

data Env = Env
  { envPort   :: !Int 
  , envPool   :: !(Pool Connection)
  , envLogger :: !Logger
  }

runApp :: Env -> App a -> IO a
runApp env = flip runReaderT env . unApp

-- | Error

-- | TODO: Refactor as https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html
data AppError
  = AppError LText
  | DbError LText
  | ValidationError (NonNull [LText])
  deriving (Show)

newtype AppException = AppException
  { unAppException :: AppError
  } deriving (Show)

instance Exception AppException

instance MonadError AppError (AppT IO) where
  throwError = liftIO . throwIO . AppException

  catchError action handler = AppT $ ReaderT $ \env -> do
    let ioAction = runApp env action
    ioAction `catch` \(AppException e) -> runApp env $ handler e

type WithError m = (MonadIO m, MonadError AppError m)

tryRunApp :: Env -> App a -> IO (Either AppError a)
tryRunApp env = fmap (first unAppException) . try . runApp env

toServerError :: AppError -> ServerError
toServerError = \case
  AppError        e -> err401 { errBody = encodeUtf8 e }
  DbError         e -> err401 { errBody = encodeUtf8 e }
  ValidationError e -> err422 { errBody = encode $ toNullable e }

-- | Logger

type Logger = String -> IO ()

class HasLogger a where
  getLogger :: a -> Logger

instance HasLogger Env where
  getLogger = envLogger

type WithLogger env m = (MonadIO m, MonadReader env m, HasLogger env)

logInfo :: (WithLogger env m, Show a) => a -> m ()
logInfo msg = do
  env <- ask
  liftIO $ getLogger env $ show msg