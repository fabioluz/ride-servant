{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ride.Auth.Class
( LoggedInUser (..)
, Login (..)
, Token (..)
) where

import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)
import Ride.Shared.Types (Id)
import Ride.User.Class (User)

-- | Represents the info of a logged in user. 
newtype LoggedInUser = LoggedInUser
  { userId :: Id User 
  } deriving (Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

-- | Represents the input format for login in the app.
data Login = Login 
  { email    :: Text
  , password :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

-- | Represents token result for login.
newtype Token = Token
  { token :: Text
  } deriving (Show, Generic, ToJSON)
