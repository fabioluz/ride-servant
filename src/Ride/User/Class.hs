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
, CreateUserInput (..)
, createUser
) where

import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON, ToJSON, toJSON, object, (.=))
import Data.Validation (Validation (..), liftError)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Ride.App (AppError (..), WithError)
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

-- | Represents the User entity in the application and database.
data User = User 
  { userId   :: Id User
  , email    :: Email
  , password :: Password
  , name     :: Text
  } deriving (Show, Generic, FromRow)

-- | Represents the input format for creating an user from an HTTP request.
data CreateUserInput = CreateUserInput
  { email    :: Text
  , password :: Text
  , name     :: Text
  } deriving (Show, Generic, FromJSON)

-- | Represents all possible errors when creating an user.
data UserError 
  = InvalidEmail EmailError
  | InvalidPassword TextError
  | InvalidName TextError
  deriving Show

-- | Validates the user input, hashes the password, generates a new UUID and returns an User Entity.
-- | If any of the steps fail, it throws an error which will be turned into an HTTP 422/500 response by Servant
createUser :: WithError m => CreateUserInput -> m User
createUser input = do
  validUser <- validateUser input
  hashedPwd <- hashPassword $ validPwd validUser
  userId    <- genNewId
  pure $ User
   { userId
   , password = hashedPwd
   , email = validEmail validUser
   , name = validName validUser
   }

-- | Validates the user input and returns ValidCreateUserInput. Throws ValidationError if validation fails.
-- | ValidationError will be turned into HTTP 422 response by Servant.
-- | HashedPassword and UserId requires IO and cannot be provided at this stage.
validateUser :: WithError m => CreateUserInput -> m ValidCreateUserInput
validateUser input = do
  validationRes <- pure $ validateCreateUser input
  case validationRes of
    Failure errs -> throwError $ validationError errs
    Success user -> pure user 

-- | Hashes the password using BCrypt. Throws an AppError in case the hashing fails (if possible?) 
hashPassword :: WithError m => Text -> m Password
hashPassword password = do
  hashRes <- liftIO $ createPassword password
  case hashRes of
    Nothing  -> throwError $ appError "error while hashing password"
    Just pwd -> pure pwd

-- | Generates a new UUID for User
genNewId :: WithError m => m (Id User)
genNewId = liftIO newId

-- | Validation

type ValidationUser a = Validation (NonNull [UserError]) a

-- | Represents a validated input for creating an user
-- | It serves as a base for generating Password and Id User, which require IO
data ValidCreateUserInput = ValidCreateUserInput
  { validEmail :: Email
  , validPwd   :: Text
  , validName  :: Text
  }

-- | Validates the email by calling Email's smart constructor and then maps the result to ValidaitonUser.
validateEmail :: Text -> ValidationUser Email
validateEmail = liftError (singleton . InvalidEmail) 
  . createEmail 

-- | Validates the password using Text validation helpers and then maps the result to ValidaitonUser.
validatePassword :: Text -> ValidationUser Text
validatePassword = liftError (singleton . InvalidPassword)
  . validateText [ minLength 6 ]

-- | Validates the name using Text validation helpers and then maps the result to ValidaitonUser.
validateName :: Text -> ValidationUser Text
validateName = liftError (singleton . InvalidName)
  . validateText [ minLength 3 ]

-- | Validates CreateUserInput and collects all the  errors by using Applicative Validation.
validateCreateUser :: CreateUserInput -> ValidationUser ValidCreateUserInput
validateCreateUser CreateUserInput {..} = do
  validEmail <- validateEmail email 
  validPwd   <- validatePassword password
  validName  <- validateName name
  pure ValidCreateUserInput {..}

-- | Error Helpers

-- | Converts collection of UserError to ValidationError
validationError :: NonNull [UserError] -> AppError
validationError = ValidationError . mapNonNull (pack . show)

-- | Constructs AppError
appError :: LText -> AppError
appError = AppError

-- | User JSON instance should not expose Password
instance ToJSON User where
  toJSON User {..} = object
    [ "userId" .= userId
    , "email" .= email
    , "name" .= name
    ]

  
