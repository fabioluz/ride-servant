{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralisedNewtypeDeriving  #-}
{-# LANGUAGE OverloadedStrings           #-}

module Ride.Shared.Types
( Id (..)
, newId
, Email
, EmailError
, createEmail
, Password (..)
, PasswordError
, createPassword 
, checkPassword
) where

import Crypto.BCrypt (hashPasswordUsingPolicy, validatePassword, slowerBcryptHashingPolicy)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)
import Data.Validation (toEither)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant (FromHttpApiData)
import Ride.Shared.Validators.Text (TextError, validateText, notEmpty, pattern)

-- | Id

newtype Id a = Id { unId :: UUID }
  deriving (Show, Generic, FromJSON, ToJSON, FromField, ToField, FromHttpApiData)

newId :: IO (Id a)
newId = Id <$> nextRandom

-- | Email

newtype Email = Email Text
  deriving (Show, Generic, FromJSON, ToJSON, FromField, ToField)

newtype EmailError = EmailError TextError
  deriving (Show)

createEmail :: Text -> Either EmailError Email
createEmail = bimap EmailError Email . validateText [ notEmpty, pattern "@" ]

-- | Password

newtype Password = Password Text
  deriving (Show, Generic, FromField, ToField)

data PasswordError = PasswordError
  deriving (Show)

createPassword :: Text -> IO (Either PasswordError Password)
createPassword = pure . toPasswordOrError <=< hashPassword . encodeUtf8
  where 
    hashPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy
    toPasswordOrError = maybe (Left PasswordError) (Right . Password . decodeUtf8)

checkPassword :: Password -> Text -> Bool
checkPassword (Password hash) plain = validatePassword (encodeUtf8 hash) (encodeUtf8 plain)