{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Ride.User.Server where

import Servant
  ( JSON
  , Get
  , Post
  , Proxy (..)
  , ReqBody
  , ServerT
  , (:>)
  , (:<|>) (..)
  , err400
  )
import Ride.App (AppT, logInfo)
import Ride.User.DB (insertUser, getAllUsers, )
import Ride.User.Class (CreateUserInput, User, createUser)

type UserAPI = "users" :> Get '[JSON] [User]
          :<|> "users" :> ReqBody '[JSON] CreateUserInput :> Post '[JSON] User

userAPI :: Proxy UserAPI
userAPI = Proxy

userServer :: ServerT UserAPI (AppT IO)
userServer = getUsers :<|> postUser

getUsers :: AppT IO [User]
getUsers = getAllUsers

postUser :: CreateUserInput -> AppT IO User
postUser userArgs = do
  user <- createUser userArgs
  insertUser user
  pure user

