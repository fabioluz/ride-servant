{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ride.Error
( GeneralError (..)
, generalError
, ValidationError (..)
, validationError
, orThrow422
, orThrow500
) where

import Data.Aeson
  ( ToJSON
  , toJSON
  , object
  , encode
  , (.=)
  )
import Servant 
  ( err500
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
  deriving Show

instance Exception GeneralError

orThrow422 :: MonadIO m => Either e a -> (e -> ValidationError) -> m a
orThrow422 e f = either (throw422 . f) pure e
  where
    throw422 (ValidationError errors) = throwIO $ err422
      { errBody = encode $ toNullable errors }

orThrow500 :: MonadIO m => Either e a -> (e -> GeneralError) -> m a
orThrow500 e f = either (throw500 . f) pure e
  where
    throw500 (GeneralError error) = throwIO $ err500
      { errBody = encodeUtf8 $ tlshow error }

validationError :: NonNull (HashMap Text Text) -> ValidationError
validationError = ValidationError

generalError :: Show e => e -> GeneralError
generalError = GeneralError . tshow