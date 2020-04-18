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
, createPassword 
) where

import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy)
import Data.Aeson (ToJSON, FromJSON)
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)
import Data.Validation (toEither)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Servant (FromHttpApiData)
import Ride.Shared.Validators.Text (TextError (..), validateText, notEmpty, pattern)

-- | Id

newtype Id a = Id { unId :: UUID }
  deriving (Show, Generic, FromJSON, ToJSON, FromField, ToField, FromHttpApiData)

newId :: IO (Id a)
newId = Id <$> nextRandom

-- | Email

newtype Email = Email { unEmail :: Text }
  deriving (Show, Generic, FromJSON, ToJSON, FromField, ToField)

newtype EmailError = EmailError TextError
  deriving (Show)

createEmail :: Text -> Either EmailError Email
createEmail = bimap EmailError Email . validateText [ notEmpty, pattern "p" ]

-- | Password

newtype Password = Password { unPassword :: Text }
  deriving (Show, Generic, FromField, ToField)

data PasswordError = PasswordError
  deriving (Show)

createPassword :: Text -> IO (Either PasswordError Password)
createPassword password = do
  hashResult <- hashPassword $ encodeUtf8 password
  pure $ maybe (Left PasswordError) (Right . Password . decodeUtf8) hashResult
  where 
    hashPassword = hashPasswordUsingPolicy slowerBcryptHashingPolicy