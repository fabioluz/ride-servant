{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Ride.User.DB where

import Database.PostgreSQL.Simple (Only (..), execute, query, query_, fromOnly)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Ride.DB (WithDb, withConn)
import Ride.Shared.Types (Id, Password (..))
import Ride.User.Class (User (..), UserWithPassword (..), ValidUpdateUser (..))

getUserByEmail :: WithDb cfg m => Text -> m (Maybe UserWithPassword)
getUserByEmail email = withConn $ \conn -> do
  results <- query conn sql args
  pure $ head <$> fromNullable results
  where
    sql = "SELECT user_id, email, password, name FROM users WHERE email = ?"
    args = Only email

getUserById :: WithDb cfg m => Id User -> m (Maybe User)
getUserById userId = withConn $ \conn -> do
  results <- query conn sql args
  pure $ head <$> fromNullable results
  where
    sql = "SELECT user_id, email, name FROM users WHERE user_id = ?"
    args = Only userId

getAllUsers :: WithDb cfg m => m [User]
getAllUsers = withConn $ \conn -> query_ conn sql
  where
    sql = "SELECT user_id, email, name FROM users"

getUserPassword :: WithDb cfg m => Id User -> m (Maybe Password)
getUserPassword userId = withConn $ \conn -> do
  results <- query conn sql args
  pure $ Password . fromOnly . head <$> fromNullable results
  where
    sql = "SELECT password FROM users WHERE user_id = ?"
    args = Only userId

insertUser :: WithDb cfg m => User -> Password -> m Int64
insertUser User {..} (Password password) = withConn $ \conn ->
  execute conn sql args
  where
    sql = "INSERT INTO users (user_id, email, password, name) \
          \VALUES (?, ?, ?, ?);"
    args = (userId, email, password, name)

updateUser :: WithDb cfg m => User -> m Int64
updateUser User {..} = withConn $ \conn ->
  execute conn sql args
  where
    sql = "UPDATE users SET email = ?, name = ? WHERE user_id = ?"
    args = (email, name, userId) 