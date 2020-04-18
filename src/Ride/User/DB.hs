{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Ride.User.DB where

import Database.PostgreSQL.Simple (execute, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Ride.DB (WithDb, withConn)
import Ride.Shared.Types (Password (..))
import Ride.User.Class (User (..), ValidUpdateUser (..))

getAllUsers :: WithDb cfg m => m [User]
getAllUsers = withConn $ \conn -> query_ conn sql
  where
    sql = "SELECT user_id, email, '', name FROM users;"

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
    sql = "UPDATE users SET email = ?, name = ? WHERE userId = ?"
    args = (email, name, userId) 