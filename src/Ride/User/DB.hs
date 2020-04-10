{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Ride.User.DB where

import Database.PostgreSQL.Simple (execute, query_)
import Database.PostgreSQL.Simple.FromRow (FromRow, fromRow, field)
import Ride.DB (WithDb, withConn)
import Ride.User.Class (User (..))

getAllUsers :: WithDb env m => m [User]
getAllUsers = withConn $ \conn -> query_ conn sql
  where
    sql = "SELECT user_id, email, '', name FROM users;"

insertUser :: WithDb env m => User -> m Int64
insertUser User {..} = withConn $ \conn -> execute conn sql args
  where
    sql = "INSERT INTO users (user_id, email, password, name) \
          \VALUES (?, ?, ?, ?)"
    args = (userId, email, password, name)
