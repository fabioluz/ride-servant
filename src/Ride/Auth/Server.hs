{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators         #-}

module Ride.Auth.Server
( Authorized
, AuthResponse
, AuthAPI
, authServer
, loginHandler
) where

import Servant 
  ( Header
  , Headers
  , JSON
  , NoContent (..)
  , Post
  , ReqBody
  , ServerT
  , (:>)
  , err401
  , err422
  )
import Servant.Auth.Server (Auth, CookieSettings, JWT, JWTSettings, SetCookie, acceptLogin)
import Ride.App (AppT)
import Ride.Auth.Class (LoggedInUser (..), Login (..))
import Ride.DB (WithDb)
import Ride.Shared.Types (Password, checkPassword)
import Ride.User.Class (UserWithPassword (..))
import Ride.User.DB (getUserByEmail)

-- Represents the type of a protected API
type Authorized = Auth '[JWT] LoggedInUser

-- Represets the response type of the login endpoint
type AuthResponse = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent

type AuthAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] AuthResponse

authServer :: MonadIO m => CookieSettings -> JWTSettings -> ServerT AuthAPI (AppT m)
authServer = loginHandler

loginHandler :: MonadIO m => CookieSettings -> JWTSettings -> Login -> AppT m AuthResponse
loginHandler cs jwts input = do
  user      <- validateUser input
  cookieRes <- liftIO $ acceptLogin cs jwts user
  case cookieRes of
    Nothing     -> throwIO err401
    Just cookie -> pure $ cookie NoContent

-- | Gets user by email and checks if it maches the provided password
validateUser :: WithDb cfg m => Login -> m LoggedInUser
validateUser login = do
  userRes <- getUserByEmail $ loginEmail login
  case userRes of
    Nothing   ->
      throwIO err401
    Just user ->
      if checkPassword (userPassword user) (loginPassword login)
        then pure $ LoggedInUser $ userId (user :: UserWithPassword)
        else throwIO err401

loginEmail :: Login -> Text
loginEmail = email :: Login -> Text

loginPassword :: Login -> Text
loginPassword = password :: Login -> Text

userPassword :: UserWithPassword -> Password
userPassword = password :: UserWithPassword -> Password

