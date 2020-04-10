{-# LANGUAGE TypeOperators #-} 

module Ride.Server where

import Control.Monad.Except (ExceptT, liftEither)
import Servant 
  ( Application
  , Proxy (..)
  , Server
  , ServerError
  , Handler (..)
  , serve
  , hoistServer
  ,
  )
import Ride.App (App, AppT (..), Env, tryRunApp, toServerError)
import Ride.User.Server (UserAPI, userAPI, userServer)

convertApp :: Env -> App a -> Handler a
convertApp env app = do
  result <- liftIO $ tryRunApp env app
  liftEither $ first toServerError result

clientToServer :: Env -> Server AppAPI
clientToServer env = hoistServer userAPI (convertApp env) userServer

type AppAPI = UserAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Env -> Application
app env = serve appApi (clientToServer env)
