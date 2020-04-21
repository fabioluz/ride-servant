{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeOperators    #-}

module Ride.User.Server where

import Data.UUID (UUID)
import Servant
  ( JSON
  , Capture
  , Get
  , Post
  , Put
  , PostCreated 
  , Proxy (..)
  , ReqBody
  , ServerT
  , (:>)
  , (:<|>) (..)
  , err400
  )
import Ride.App (AppT, App, logInfo)
import Ride.Error (orThrow422, validationError)
import Ride.Shared.Types (Id (..))
import Ride.User.Class
  ( CreateUser (..)
  , UpdateUser (..)
  , User
  , createUser
  , createUserPassword
  , updateUser
  , newUserId
  )

import qualified Ride.User.DB as DB

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] CreateUser :> PostCreated '[JSON] User
          :<|> "users" :> Capture "userId" (Id User) :> ReqBody '[JSON] UpdateUser :> Put '[JSON] ()

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = getUsers :<|> postUser :<|> putUser

getUsers :: MonadIO m => AppT m [User]
getUsers = DB.getAllUsers

postUser :: MonadIO m => CreateUser -> AppT m User
postUser input = do
  userId   <- newUserId
  user     <- createUser userId input `orThrow422` validationError
  password <- createUserPassword input
  DB.insertUser user password
  pure user

putUser :: MonadIO m => Id User -> UpdateUser -> AppT m ()
putUser userId input = do
  user <- updateUser userId input `orThrow422` validationError
  DB.updateUser user
  pure ()
  
