{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ride.Error
( GeneralError (..)
, ValidationError (..)
, errorHandlers
) where

import ClassyPrelude (Handler (..))
import Data.Aeson (ToJSON, toJSON, object, encode, (.=), Object, Value)
import Database.PostgreSQL.Simple
  ( SqlError
  , FormatError
  , QueryError
  , ResultError
  )
import Servant 
  ( ServerError
  , err500
  , err422
  , errBody
  )
import Ride.App (Config (..), Env (..))

newtype ValidationError = ValidationError (NonNull (HashMap Text Text))
  deriving Show

instance ToJSON ValidationError where
  toJSON (ValidationError errors) = object [ "errors" .= toNullable errors ]

instance Exception ValidationError

newtype GeneralError = GeneralError Text
  deriving (Show)

instance Exception GeneralError

throw500 :: Exception e => Env -> e -> IO a
throw500 env ex = case env of
  Development -> throwIO $ err500 { errBody = encodeUtf8 $ tlshow ex }
  Production  -> throwIO $ err500 { errBody = "An error has occurred." }
 
errorHandlers :: Config -> [Handler IO a]
errorHandlers cfg = 
  let env = configEnv cfg in
  [ Handler $ \(ex :: SqlError)        -> throw500 env ex
  , Handler $ \(ex :: FormatError)     -> throw500 env ex
  , Handler $ \(ex :: QueryError)      -> throw500 env ex
  , Handler $ \(ex :: ResultError)     -> throw500 env ex
  , Handler $ \(ex :: GeneralError)    -> throw500 env ex
  , Handler $ \(ex :: ValidationError) -> throwIO $ err422 { errBody = encode ex }
  ]