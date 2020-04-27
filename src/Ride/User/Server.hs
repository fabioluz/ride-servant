{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module Ride.User.Server (UserAPI, userServer) where

import Servant
  ( JSON
  , Capture
  , Get
  , Post
  , Put
  , PostCreated 
  , ReqBody
  , ServerT
  , (:>)
  , (:<|>) (..)
  , err401
  )
import Servant.Auth.Server (AuthResult (..), throwAll)
import Ride.App (AppT, logInfo)
import Ride.Auth.Class (LoggedInUser (..))
import Ride.Error (orThrow422, validationError)
import Ride.Shared.Types (Id (..))
import Ride.User.Class
  ( CreateUser (..)
  , UpdateUser (..)
  , User
  , createUser
  , createUserPassword
  , newUserId
  , updateUser
  )

import qualified Ride.User.DB as DB

type UserAPI = "me" :> Get '[JSON] User
          :<|> "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] CreateUser :> PostCreated '[JSON] User
          :<|> "users" :> Capture "userId" (Id User) :> ReqBody '[JSON] UpdateUser :> Put '[JSON] ()

userServer :: (MonadIO m) => AuthResult LoggedInUser -> ServerT UserAPI (AppT m)
userServer (Authenticated user)
     = getMeHandler user
  :<|> getUsersHandler
  :<|> createUserHandler
  :<|> updateUserHandler

userServer _ = throwAll err401

getMeHandler :: MonadIO m => LoggedInUser -> AppT m User
getMeHandler user = do
  userRes <- DB.getUserById $ userId user
  maybe (throwIO err401) pure userRes

getUsersHandler :: MonadIO m => AppT m [User]
getUsersHandler = DB.getAllUsers

createUserHandler :: MonadIO m => CreateUser -> AppT m User
createUserHandler input = do
  userId   <- newUserId
  user     <- createUser userId input `orThrow422` validationError
  password <- createUserPassword input
  DB.insertUser user password
  pure user

updateUserHandler :: MonadIO m => Id User -> UpdateUser -> AppT m ()
updateUserHandler userId input = do
  user <- updateUser userId input `orThrow422` validationError
  DB.updateUser user
  pure ()