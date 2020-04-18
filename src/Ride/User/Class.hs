{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Ride.User.Class
( User (..)
, UserError (..)
, CreateUser (..)
, UpdateUser (..)
, ValidUpdateUser (..)
, createUser
, updateUser
) where

import Control.Monad.Except (throwError)
import Data.Aeson (Value, FromJSON, ToJSON, toJSON, encode, object, (.=))
import Data.Validation (Validation (..), liftError, validation)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Ride.Error (GeneralError (..), ValidationError (..))
import Ride.Shared.Types 
  ( Id 
  , Email
  , EmailError
  , Password
  , newId
  , createEmail
  , createPassword
  )
import Ride.Shared.Validators.Text
  ( TextError
  , validateText
  , notEmpty
  , minLength
  )

import qualified Data.HashMap.Strict as HM

-- | Represents the User entity in the application and database.
-- | This type does not contain password information.
-- | It is used for all user operations.
data User = User 
  { userId   :: Id User
  , email    :: Email
  , name     :: Text
  } deriving (Show, Generic, ToJSON, FromRow)

-- | Represents the input format for creating an user from an HTTP request.
data CreateUser = CreateUser
  { email    :: Text
  , password :: Text
  , name     :: Text
  } deriving (Show, Generic, FromJSON)

-- | Represents a validated input for creating an user
-- | It serves as a base for generating Password and Id User, which require IO
data ValidCreateUser = ValidCreateUser
  { createUserEmail    :: Email
  , createUserPassword :: Text
  , createUserName     :: Text
  }

-- | Represents the input format for updating an user from an HTTP request.
data UpdateUser = UpdateUser
  { email :: Text
  , name  :: Text
  } deriving (Show, Generic, FromJSON)

-- | Represents a validated input for updating an user.
data ValidUpdateUser = ValidUpdateUser
  { updateUserEmail :: Email
  , updateUserName  :: Text
  }

-- | Validates the user input, hashes the password, generates a new UUID and returns an User entity and Password.
-- | If any of the steps fail, it throws an error which will be turned into an HTTP 422/500 response by Servant.
createUser :: MonadIO m => CreateUser -> m (User, Password)
createUser input = do
  validUser <- validateCreateUser input
  password  <- hashPassword $ createUserPassword validUser
  userId    <- genNewId
  let email = createUserEmail validUser
      name  = createUserName validUser
      user  = User {..}
  pure $ (User {..}, password)

-- | Validates the user input and returns an User entity.
-- | If any of the steps fail, it throws an error which will be turned into an HTTP 422/500 response by Servant
updateUser :: MonadIO m => Id User -> UpdateUser -> m User
updateUser userId input = do
  validUser <- validateUpdateUser input
  let email = updateUserEmail validUser
      name  = updateUserName validUser
  pure $ User {..}

-- | Hashes the password using BCrypt. Throws an GeneralError in case the hashing fails (if possible?) 
hashPassword :: MonadIO m => Text -> m Password
hashPassword password = do
  hashResult <- liftIO $ createPassword password
  either (throwIO . generalError) pure hashResult

-- | Generates a new UUID for User
genNewId :: MonadIO m => m (Id User)
genNewId = liftIO newId

-- |
-- | Validation 
-- |

-- | Validates the user input and returns ValidCreateUser. Throws ValidationError if validation fails.
-- | ValidationError will be turned into HTTP 422 response by Servant.
-- | HashedPassword and UserId cannot be provided at this stage.
validateCreateUser :: MonadIO m => CreateUser -> m ValidCreateUser
validateCreateUser = validation (throwIO . validationError) pure . validate
  where
    -- | Validates CreateUser and collects all the errors by using Applicative Validation.
    validate :: CreateUser -> Validation UserError ValidCreateUser
    validate CreateUser {..} = do
      createUserEmail    <- validateEmail email 
      createUserPassword <- validatePassword password
      createUserName     <- validateName name
      pure ValidCreateUser {..}

-- | Validates the user input and returns ValidUpdateUserInput. Throws ValidationError if validation fails.
-- | ValidationError will be turned into HTTP 422 response by Servant.
validateUpdateUser :: MonadIO m => UpdateUser -> m ValidUpdateUser
validateUpdateUser = validation (throwIO . validationError) pure . validate
  where
    -- | Validates UpdateUserInput and collects all the errors by using Applicative Validation.
    validate :: UpdateUser -> Validation UserError ValidUpdateUser
    validate UpdateUser {..} = do
      updateUserEmail <- validateEmail email 
      updateUserName  <- validateName name
      pure ValidUpdateUser {..}

-- | Validates the email by calling Email's smart constructor and then maps the result to ValidaitonUser.
validateEmail :: Text -> Validation UserError  Email
validateEmail = liftError emailError . createEmail 

-- | Validates the password using Text validation helpers and then maps the result to ValidaitonUser.
validatePassword :: Text -> Validation UserError Text
validatePassword = liftError passwordError . validateText [ minLength 6 ]

-- | Validates the name using Text validation helpers and then maps the result to ValidaitonUser.
validateName :: Text -> Validation UserError Text
validateName = liftError nameError . validateText [ minLength 3 ]

-- | Type synonym for Validation NonNull (HashMap Text Text)
-- | It is a non null collection of hash maps - key and error message
type UserError = NonNull (HashMap Text Text)

createUserError :: Text -> Text -> UserError
createUserError key = impureNonNull . HM.singleton key

emailError :: EmailError -> UserError
emailError = createUserError "email" . tshow

passwordError :: TextError -> UserError
passwordError = createUserError "password" . tshow

nameError :: TextError -> UserError
nameError = createUserError "name" . tshow

-- | Converts collection of UserError to ValidationError
validationError :: UserError -> ValidationError 
validationError = ValidationError

-- | Constructs AppError
generalError :: Show a => a -> GeneralError
generalError = GeneralError . tshow 

  
