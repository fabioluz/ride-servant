{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Ride.User.Class
( User (..)
, newUserId
, CreateUser (..)
, createUser
, UserWithPassword (..)
, createUserPassword
, UpdateUser (..)
, updateUser
) where

import Data.Validation (Validation (..), liftError, validation, toEither)
import Ride.Error (generalError)
import Ride.Shared.Types 
  ( Id 
  , Email
  , EmailError
  , Password
  , PasswordError
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
  { userId :: Id User
  , email  :: Email
  , name   :: Text
  } deriving (Show, Generic, ToJSON, FromRow)

-- | Represents the User entity with Password
-- | It should only be used for authentication.
data UserWithPassword = UserWithPassword
  { userId   :: Id User
  , email    :: Email
  , password :: Password
  , name     :: Text
  } deriving (Show, Generic, FromRow)



-- |
-- | Create User
-- |

-- | Represents the input format for creating an user from an HTTP request.
data CreateUser = CreateUser
  { email    :: Text
  , password :: Text
  , name     :: Text
  } deriving (Show, Generic, FromJSON)

-- | Represents a validated input for creating an user
-- | It serves as a base for generating Password, which requires IO
data ValidCreateUser = ValidCreateUser
  { email    :: Email
  , password :: Text
  , name     :: Text
  }

-- | Validates the create user input and returns either UserError or User entity.
createUser :: Id User -> CreateUser -> Either UserError User
createUser userId = second toUser . validateCreateUser
  where
    toUser ValidCreateUser {..} = User {..}

-- |
-- | Update User
-- |

-- | Represents the input format for updating an user from an HTTP request.
data UpdateUser = UpdateUser
  { email :: Text
  , name  :: Text
  } deriving (Show, Generic, FromJSON)

-- | Represents a validated input for updating an user.
data ValidUpdateUser = ValidUpdateUser
  { email :: Email
  , name  :: Text
  }

-- | Validates the update user input and returns either UserError or User entity.
updateUser :: Id User -> UpdateUser -> Either UserError User
updateUser userId = second toUser . validateUpdateUser
  where
    toUser ValidUpdateUser {..} = User {..}

-- |
-- | Password
-- |

-- | Creates a bcrypt hashed password from create user input.
createUserPassword :: MonadIO m => CreateUser -> m Password
createUserPassword = passwordOrThrow <=< liftIO . createPassword . getPassword
  where
    getPassword = password :: CreateUser -> Text
    passwordOrThrow = either (throwIO . generalError . tshow) pure

-- |
-- | Id
-- |

-- | Generates a new UUID for User
newUserId :: MonadIO m => m (Id User)
newUserId = liftIO newId

-- |
-- | Validation 
-- |

-- | Validates the user input and returns ValidCreateUser.
validateCreateUser :: CreateUser -> Either UserError ValidCreateUser
validateCreateUser = toEither . validate
  where
    -- | Validates CreateUser and collects all the errors by using Applicative Validation.
    validate :: CreateUser -> Validation UserError ValidCreateUser
    validate CreateUser {..} = do
      email    <- validateEmail email 
      password <- validatePassword password
      name     <- validateName name
      pure ValidCreateUser {..}

-- | Validates the user input and returns ValidUpdateUserInput.
validateUpdateUser :: UpdateUser -> Either UserError ValidUpdateUser
validateUpdateUser = toEither . validate
  where
    -- | Validates UpdateUserInput and collects all the errors by using Applicative Validation.
    validate :: UpdateUser -> Validation UserError ValidUpdateUser
    validate UpdateUser {..} = do
      email <- validateEmail email 
      name  <- validateName name
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
  