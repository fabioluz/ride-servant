{-# LANGUAGE OverloadedStrings #-}

module Lib (main) where

import Network.Wai.Handler.Warp (run)
import Servant (Context (..))
import Servant.Auth.Server (JWTSettings (..), defaultJWTSettings, defaultCookieSettings, generateKey)
import System.Environment (lookupEnv)
import Ride.App (Config (..), Env (..), lookupSetting)
import Ride.DB (acquirePool, destroyPool, migrateDb)
import Ride.Server (app)

main :: IO ()
main = bracket acquireConfig shutdownApp startApp

startApp :: Config -> IO ()
startApp cfg = do
  key <- generateKey
  let jwt     = defaultJWTSettings key
      cookie  = defaultCookieSettings
      context = cookie :. jwt :. EmptyContext
      pool    = configPool cfg
      port    = configPort cfg

  migrateDb pool
  run port $ app context cookie jwt cfg

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  destroyPool (configPool cfg)
  pure () 

acquireConfig :: IO Config
acquireConfig = do
  port <- lookupSetting "PORT" 8090
  env  <- lookupSetting "ENV" Development
  pool <- acquirePool
  pure Config
    { configPool   = pool
    , configEnv    = env
    , configPort   = port 
    , configLogger = print
    }

