{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Ride.Auth.Class where

import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

data AuthUser = AuthUser
  { id :: Text 
  } deriving (Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

data Login = Login 
  { user     :: Text
  , password :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

