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
  , NoContent (..)
  , (:>)
  , (:<|>) (..)
  , err403
  )
import Servant.Auth.Server (AuthResult (..), throwAll)
import Ride.App (AppT)
import Ride.Auth.Class (LoggedInUser (..))
import Ride.Error (orThrow, validationError)
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

type UserAPI
    =  "me"
      :> Get '[JSON] User
  :<|> "users"
      :> Get '[JSON] [User]
  :<|> "users"
      :> ReqBody '[JSON] CreateUser
      :> PostCreated '[JSON] User
  :<|> "users"
      :> Capture "userId" (Id User)
      :> ReqBody '[JSON] UpdateUser
      :> Put '[JSON] NoContent

userServer :: (MonadIO m) => AuthResult LoggedInUser -> ServerT UserAPI (AppT m)
userServer (Authenticated user)
    =  getMeHandler user
  :<|> getUsersHandler
  :<|> createUserHandler
  :<|> updateUserHandler

userServer _ = throwAll err403

getMeHandler :: MonadIO m => LoggedInUser -> AppT m User
getMeHandler user = do
  userRes <- DB.getUserById $ userId user
  maybe (throwIO err403) pure userRes

getUsersHandler :: MonadIO m => AppT m [User]
getUsersHandler = DB.getAllUsers

createUserHandler :: MonadIO m => CreateUser -> AppT m User
createUserHandler input = do
  userId   <- newUserId
  user     <- createUser userId input `orThrow` validationError
  password <- createUserPassword input
  DB.insertUser user password
  pure user

updateUserHandler :: MonadIO m => Id User -> UpdateUser -> AppT m NoContent
updateUserHandler userId input = do
  user <- updateUser userId input `orThrow` validationError
  DB.updateUser user
  pure NoContent