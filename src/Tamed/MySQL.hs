{-# LANGUAGE Trustworthy #-}

module Tamed.MySQL (
    Connection, ConnectInfo, Only(..),
    defaultConnectInfo, connectUser, connectPassword, connectDatabase,
    query_)
where

import Database.MySQL.Simple (
    Connection, ConnectInfo, Only(..),
    defaultConnectInfo, connectUser, connectPassword, connectDatabase,
    query_)
