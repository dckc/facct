{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Safe #-}

module DBAccess ( cliAccess, dbJob ) where

import Tamed.Keyring (
    Network, NetworkPassword, Operation, KeyringError,
    findNetworkPassword,
    network, networkUser, networkProtocol, networkServer, networkObject,
    networkPasswordNetwork, networkPasswordSecret)
import Tamed.MySQL (
    Connection, ConnectInfo, Only(..),
    defaultConnectInfo, connectUser, connectPassword, connectDatabase,
    query_)


dbJob :: Connection -> IO Int
dbJob conn = do
   [Only i] <- query_ conn "select count(*) from transactions"
   return i

data Problem = Usage String
    | LookupFailure String
    | KeyRingAccess KeyringError
  deriving Show


-- Connect to the DB indicated by CLI args with credentials from Gnome Keyring
cliAccess :: (IO [String]) -- access to CLI args
    -> (Operation [NetworkPassword] -> IO (Either KeyringError [NetworkPassword])) -- access to gnome keyring
    -> (ConnectInfo -> IO Connection) -- access to DB
    -> IO (Either Problem Connection)
cliAccess getArgs sync connect = do
    args <- getArgs
    info <- findCreds $ cli args
    conn <- dbAccess info
    return conn

    where

    findCreds :: (Either Problem String) -> IO (Either Problem ConnectInfo)
    findCreds (Right dbname) = do
        answers <- sync $ findNetworkPassword $ dbloc dbname
        return $ check answers
        where

        check :: Either KeyringError [NetworkPassword] -> Either Problem ConnectInfo
        check (Right (c : _cs)) = checkUser $ networkUser $ networkPasswordNetwork c
            where
            checkUser (Just who) = Right $ dbInfo dbname who (networkPasswordSecret c)
            checkUser Nothing = Left $ LookupFailure "no username"
        check (Right []) = Left $ LookupFailure ("no passwords for " ++ dbname)
        check (Left oops) = Left $ KeyRingAccess oops
    findCreds (Left oops) = return $ Left oops

    dbAccess :: Either Problem ConnectInfo -> IO (Either Problem Connection)
    dbAccess (Right info) = do
        conn <- connect info
        return $ Right conn
    dbAccess (Left oops) = return $ Left oops


cli :: [String] -> Either Problem String
cli [db] = Right db
cli _ = Left $ Usage "usage: prog dbname"

dbloc dbname = network {
            networkProtocol = Just "mysql",
            networkServer = Just "localhost",
            networkObject = Just dbname
        }

dbInfo dbname username password = defaultConnectInfo {
    connectUser = username,
    connectPassword = password,
    connectDatabase = dbname
}
