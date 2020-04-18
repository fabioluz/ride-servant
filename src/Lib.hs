module Lib (main) where

import Network.Wai.Handler.Warp (run)
import Ride.App (Config (..), Env (..))
import Ride.DB (acquirePool, destroyPool, migrateDb)
import Ride.Server (app)

main :: IO ()
main = bracket acquireEnv shutdownApp startApp

startApp :: Config -> IO ()
startApp cfg = do
  migrateDb (configPool cfg)
  run (configPort cfg) (app cfg)

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
  destroyPool (configPool cfg)
  pure () 

acquireEnv :: IO Config
acquireEnv = do
  pool <- acquirePool
  pure Config
    { configPool   = pool
    , configEnv    = Development
    , configPort   = 8090 
    , configLogger = print
    }

