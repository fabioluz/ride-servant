{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Ride.Auth.Server where

import Servant (JSON, Post, Proxy (..), ReqBody, ServerT, (:>))
import Servant.Auth.Server (JWTSettings)
import Ride.App (AppT)
import Ride.Auth.Class (Login)

type AuthAPI = "login" :> ReqBody '[JSON] Login :> Post '[JSON] Text

userAPI :: Proxy AuthAPI
userAPI = Proxy

loginHandler :: MonadIO m => JWTSettings -> Login -> AppT m Text
loginHandler jwts input = pure ""