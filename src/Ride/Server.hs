{-# LANGUAGE TypeOperators #-} 

module Ride.Server (app) where

import Control.Monad.Except (ExceptT (..), liftEither)
import Servant 
  ( Application
  , Proxy (..)
  , Server
  , ServerError
  , Handler (..)
  , serve
  , hoistServer
  )
import Ride.App (AppT (..), Config)
import Ride.Error (errorHandlers)
import Ride.User.Server (UserAPI, userAPI, userServer)

runApp :: Config -> AppT IO a -> IO a
runApp cfg app = runReaderT (unApp app) cfg `catches` (errorHandlers cfg)

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg app = Handler . ExceptT . try $ runApp cfg app

clientToServer :: Config -> Server AppAPI
clientToServer cfg = hoistServer userAPI (convertApp cfg) userServer

type AppAPI = UserAPI

appApi :: Proxy AppAPI
appApi = Proxy

app :: Config -> Application
app cfg = serve appApi (clientToServer cfg)
