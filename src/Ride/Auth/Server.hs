{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}

module Ride.Auth.Server
( AuthResponse
, AuthAPI
, authServer
, loginHandler
) where

import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Data.Time.Clock (addUTCTime)
import Servant 
  ( Header
  , Headers
  , JSON
  , Post
  , ReqBody
  , ServerT
  , (:>)
  , addHeader
  , err401
  )
import Servant.Auth.Server
  ( Auth
  , CookieSettings
  , JWT
  , JWTSettings
  , SetCookie
  , cookieExpires
  , makeSessionCookie
  , makeXsrfCookie
  )
import Web.Cookie (setCookieValue)
import Ride.App (AppT)
import Ride.Auth.Class (LoggedInUser (..), Login (..), Token (..))
import Ride.DB (WithDb)
import Ride.User.DB (getUserByEmail)
import Ride.User.Class (UserWithPassword (..))
import Ride.Shared.Types (Password, checkPassword)

-- Represets the response type of the login endpoint
type AuthResponse = Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] Token

-- The contract of the Authentication API. This API is public
type AuthAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] AuthResponse

authServer :: MonadIO m => CookieSettings -> JWTSettings -> ServerT AuthAPI (AppT m)
authServer = loginHandler

-- | This checks user's credentials and generates JWT token if it is valid.
-- | The token is generated as cookie as well as an JSON response.
loginHandler :: MonadIO m => CookieSettings -> JWTSettings -> Login -> AppT m AuthResponse
loginHandler baseCS jwts input = do
  cs      <- updateCS baseCS
  xsrf    <- genXsrfToken cs
  mayAuth <- genAuthToken cs jwts input
  case mayAuth of
    Nothing   -> throwIO err401
    Just auth -> pure 
      . addHeader auth 
      . addHeader xsrf 
      . Token
      . decodeUtf8
      . setCookieValue
      $ auth

-- | Updates cookie settings' expiration time
-- | This is also used in JWT tokens internally
updateCS :: MonadIO m => CookieSettings -> m CookieSettings
updateCS cs = do
  now <- liftIO getCurrentTime
  let exp = Just $ addUTCTime (43800 * 60) now
  pure $ cs { cookieExpires = exp }

-- | Generates a cookie for XSRF protection
genXsrfToken :: MonadIO m => CookieSettings -> m SetCookie
genXsrfToken = liftIO . makeXsrfCookie

-- | Generates a cookie for the Authorization Token
genAuthToken :: (MonadIO m, WithDb cfg m) => CookieSettings -> JWTSettings -> Login -> m (Maybe SetCookie)
genAuthToken cs jwts input = runMaybeT $ do
  user         <- MaybeT $ getUserByEmail (loginEmail input)
  loggedInUser <- MaybeT $ validatePassword input user
  session      <- MaybeT $ liftIO (makeSessionCookie cs jwts loggedInUser)
  pure session

-- | Checks if provided password matches with user data
validatePassword :: MonadIO m => Login -> UserWithPassword -> m (Maybe LoggedInUser)
validatePassword login user = pure $
  if checkPassword (loginPassword login) (userPassword user)
    then Just $ mkLoggedInUser user
    else Nothing
    
-- |
-- | Utilities
-- |

mkLoggedInUser :: UserWithPassword -> LoggedInUser
mkLoggedInUser UserWithPassword {..} = LoggedInUser {..}

loginEmail :: Login -> Text
loginEmail = email :: Login -> Text

loginPassword :: Login -> Text
loginPassword = password :: Login -> Text

userPassword :: UserWithPassword -> Password
userPassword = password :: UserWithPassword -> Password

