{-# LANGUAGE OverloadedStrings #-}
module DBAccess ( cliAccess, dbJob ) where

import Gnome.Keyring (Network, NetworkPassword, Operation, KeyringError,
                        sync,
                        network, networkUser, networkProtocol, networkServer, networkObject,
                        networkPasswordNetwork, networkPasswordSecret)
import Database.MySQL.Simple (Connection, ConnectInfo, Only(..),
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
    -> (Network -> Operation [NetworkPassword]) -- lookup access to gnome keyring
    -> (ConnectInfo -> IO Connection)
    -> IO (Either Problem Connection)
cliAccess getArgs find connect = do
    args <- getArgs
    info <- findCreds $ cli args
    conn <- dbAccess info
    return conn

    where

    findCreds :: (Either Problem String) -> IO (Either Problem ConnectInfo)
    findCreds (Right dbname) = do
        answers <- sync $ find $ dbloc dbname
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
