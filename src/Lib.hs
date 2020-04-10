module Lib (main) where

import Network.Wai.Handler.Warp (run)
import Ride.App (Env (..))
import Ride.DB (acquirePool, destroyPool, migrateDb)
import Ride.Server (app)

main :: IO ()
main = bracket acquireEnv shutdownApp startApp

startApp :: Env -> IO ()
startApp env = do
  migrateDb (envPool env)
  run (envPort env) (app env)

shutdownApp :: Env -> IO ()
shutdownApp env = do
  destroyPool (envPool env)
  pure () 

acquireEnv :: IO Env
acquireEnv = do
  pool <- acquirePool
  pure Env
    { envPool   = pool
    , envPort   = 8090 
    , envLogger = print
    }

