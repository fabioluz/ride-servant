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
import Servant.Auth.Server (AuthResult (..))
import Ride.App (AppT, logInfo)
import Ride.Auth.Class (LoggedInUser (..))
import Ride.Auth.Server (Authorized)
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

type UserAPI = Authorized :> "me" :> Get '[JSON] User
          :<|> Authorized :> "users" :> Get '[JSON] [User]
          :<|> Authorized :> "users" :> ReqBody '[JSON] CreateUser :> PostCreated '[JSON] User
          :<|> Authorized :> Capture "userId" (Id User) :> ReqBody '[JSON] UpdateUser :> Put '[JSON] ()

userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = getMeHandler
        :<|> getUsersHandler
        :<|> createUserHandler
        :<|> updateUserHandler

-- withAuth :: MonadIO m => AuthResult LoggedInUser -> (LoggedInUser -> AppT m a) -> AppT m a
-- withAuth (Authenticated user) f = f user
-- withAuth _ _                    = throwIO err401

getMeHandler :: MonadIO m => AuthResult LoggedInUser -> AppT m User
getMeHandler (Authenticated user) = do
  userRes <- DB.getUserById $ userId user
  maybe (throwIO err401) pure userRes

getMeHandler _ = throwIO err401

getUsersHandler :: MonadIO m => AuthResult LoggedInUser -> AppT m [User]
getUsersHandler (Authenticated _) = DB.getAllUsers

getUsersHandler _ = throwIO err401

createUserHandler :: MonadIO m => AuthResult LoggedInUser -> CreateUser -> AppT m User
createUserHandler (Authenticated _) input = do
  userId   <- newUserId
  user     <- createUser userId input `orThrow422` validationError
  password <- createUserPassword input
  DB.insertUser user password
  pure user

createUserHandler _ _ = throwIO err401

updateUserHandler :: MonadIO m => AuthResult LoggedInUser -> Id User -> UpdateUser -> AppT m ()
updateUserHandler (Authenticated _) userId input = do
  user <- updateUser userId input `orThrow422` validationError
  DB.updateUser user
  pure ()
  
updateUserHandler _ _ _ = throwIO err401