{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ride.User.DB where

import Database.PostgreSQL.Simple (Only (..), execute, query, query_, fromOnly)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Ride.DB (WithDb, withConn)
import Ride.Shared.Types (Id, Password (..))
import Ride.User.Class (User (..), UserWithPassword (..))

getUserByEmail :: WithDb cfg m => Text -> m (Maybe UserWithPassword)
getUserByEmail email = withConn $ \conn -> do
  results <- query conn userSQL userArgs
  pure $ head <$> fromNullable results
  where
    userSQL = [sql|
       SELECT user_id, email, password, name
       FROM users
       WHERE email = ?
    |]
    userArgs = Only email

getUserById :: WithDb cfg m => Id User -> m (Maybe User)
getUserById userId = withConn $ \conn -> do
  results <- query conn userSQL userArgs
  pure $ head <$> fromNullable results
  where
    userSQL = [sql|
      SELECT user_id, email, name
      FROM users
      WHERE user_id = ?
    |]
    userArgs = Only userId

getAllUsers :: WithDb cfg m => m [User]
getAllUsers = withConn $ \conn -> query_ conn usersSQL
  where
    usersSQL = [sql| SELECT user_id, email, name FROM users |]

getUserPassword :: WithDb cfg m => Id User -> m (Maybe Password)
getUserPassword userId = withConn $ \conn -> do
  results <- query conn userSQL userArgs
  pure $ Password . fromOnly . head <$> fromNullable results
  where
    userSQL = [sql| SELECT password FROM users WHERE user_id = ? |]
    userArgs = Only userId

insertUser :: WithDb cfg m => User -> Password -> m Int64
insertUser User {..} (Password password) = withConn $ \conn ->
  execute conn userSQL userArgs
  where
    userSQL = [sql|
      INSERT INTO users (user_id, email, password, name) 
      VALUES (?, ?, ?, ?)
    |]
    userArgs = (userId, email, password, name)

updateUser :: WithDb cfg m => User -> m Int64
updateUser User {..} = withConn $ \conn ->
  execute conn userSQL userArgs
  where
    userSQL = [sql|
      UPDATE users SET email = ?, name = ?
      WHERE user_id = ?
    |]
    userArgs = (email, name, userId) 