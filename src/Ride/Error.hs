{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ride.Error
( generalError
, validationError
, orThrow
) where

import Data.Aeson (toJSON, object, encode, (.=))
import Servant (ServerError, err500, err422, errBody)

newtype ValidationError = ValidationError (NonNull (HashMap Text Text))
  deriving Show

instance ToJSON ValidationError where
  toJSON (ValidationError errors) = object [ "errors" .= toNullable errors ]

instance Exception ValidationError

newtype GeneralError = GeneralError Text
  deriving Show

instance Exception GeneralError

orThrow :: MonadIO m => Either e a -> (e -> ServerError) -> m a
orThrow e f = either (throwIO . f) pure e

validationError :: NonNull (HashMap Text Text) -> ServerError
validationError errors = err422
  { errBody = encode . ValidationError $ errors }

generalError :: Show e => e -> ServerError
generalError error = err500
  { errBody = encodeUtf8 $ tlshow error }