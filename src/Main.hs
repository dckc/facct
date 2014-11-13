{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Exit
import qualified System.Environment as Sys
import qualified Gnome.Keyring as GK
import qualified Database.MySQL.Simple as DB

main :: IO ()
main = do
    maybeAccess <- cliAccess Sys.getArgs GK.findNetworkPassword
    result <- case maybeAccess of
        Right conn -> do
            ans <- dbJob conn
            return $ Right ans
        Left oops -> return $ Left oops
    report result
    where
        report (Right ans) = do
            print ans

        report (Left oops) = do
            hPrint stderr $ show oops
            exitFailure

dbJob :: DB.Connection -> IO Int
dbJob conn = do
   [DB.Only i] <- DB.query_ conn "select count(*) from transactions"
   return i

data Problem = Usage String
    | NoUsername
    | NoMatchingPasswords String
    | KeyRingError GK.KeyringError
  deriving Show


-- Connect to the DB indicated by CLI args with credentials from Gnome Keyring
cliAccess :: (IO [String]) -- access to CLI args
    -> (GK.Network -> GK.Operation [GK.NetworkPassword]) -- lookup access to gnome keyring
    -> IO (Either Problem DB.Connection)
cliAccess getArgs find = do
    args <- getArgs
    info <- findCreds $ cli args
    conn <- dbAccess info
    return conn

    where

    findCreds :: (Either Problem String) -> IO (Either Problem DB.ConnectInfo)
    findCreds (Right dbname) = do
        answers <- GK.sync $ find $ dbloc dbname
        return $ check answers
        where

        check :: Either GK.KeyringError [GK.NetworkPassword] -> Either Problem DB.ConnectInfo
        check (Right (c : _cs)) = checkUser $ GK.networkUser $ GK.networkPasswordNetwork c
            where
            checkUser (Just who) = Right $ dbInfo dbname who (GK.networkPasswordSecret c)
            checkUser Nothing = Left NoUsername
        check (Right []) = Left $ NoMatchingPasswords dbname
        check (Left oops) = Left $ KeyRingError oops
    findCreds (Left oops) = return $ Left oops

    dbAccess :: Either Problem DB.ConnectInfo -> IO (Either Problem DB.Connection)
    dbAccess (Right info) = do
        conn <- DB.connect info
        return $ Right conn
    dbAccess (Left oops) = return $ Left oops


cli :: [String] -> Either Problem String
cli [db] = Right db
cli _ = Left $ Usage "usage: prog dbname"

dbloc dbname = GK.network {
            GK.networkProtocol = Just "mysql",
            GK.networkServer = Just "localhost",
            GK.networkObject = Just dbname
        }

dbInfo dbname username password = DB.defaultConnectInfo {
    DB.connectUser = username,
    DB.connectPassword = password,
    DB.connectDatabase = dbname
}

--	GLib.setApplicationName "GNOME Keyring example"
--	GLib.setProgramName "/path/to/binary"
