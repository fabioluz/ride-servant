{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-} 
{-# LANGUAGE RankNTypes       #-}

module Ride.Server (app) where

import Control.Monad.Except (ExceptT (..))
import Servant 
  ( Application
  , Context
  , Handler (..)
  , Proxy (..)
  , Server
  , ServerT
  , serveWithContext
  , hoistServerWithContext
  , (:<|>) (..)
  )
import Servant.Auth.Server (AuthResult, CookieSettings, JWTSettings)
import Ride.App (AppT (..), Config)
import Ride.Auth.Class (LoggedInUser)
import Ride.Auth.Server (AuthAPI, authServer)
import Ride.User.Server (UserAPI, userServer)

type AppContext = '[CookieSettings, JWTSettings]

proxyContext :: Proxy AppContext
proxyContext = Proxy

type AppAPI = AuthAPI :<|> UserAPI

proxyAPI :: Proxy AppAPI
proxyAPI = Proxy

runApp :: Config -> AppT IO a -> IO a
runApp cfg = flip runReaderT cfg . unApp

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg = Handler . ExceptT . try . runApp cfg

server :: MonadIO m => CookieSettings -> JWTSettings -> ServerT AppAPI (AppT m)
server cs jwts = authServer cs jwts :<|> userServer

hoistServer :: CookieSettings -> JWTSettings -> Config -> Server AppAPI
hoistServer cs jwts cfg = hoistServerWithContext
  proxyAPI proxyContext (convertApp cfg) (server cs jwts)

app :: Context AppContext -> CookieSettings -> JWTSettings -> Config -> Application
app ctx cs jwts = serveWithContext proxyAPI ctx . hoistServer cs jwts
