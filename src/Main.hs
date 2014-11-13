module Main where

import System.IO
import System.Exit
import qualified System.Environment as Sys
import qualified Gnome.Keyring as GK

main :: IO ()
main = do
    maybeAccess <- cliAccess Sys.getArgs GK.findNetworkPassword
    report maybeAccess
    where
        report (Right creds) = print creds
        report (Left oops) = do
            hPrint stderr $ show oops
            exitFailure

data Problem = Usage String
    | NoUsername
    | NoMatchingPasswords String
    | KeyRingError GK.KeyringError
  deriving Show

cliAccess :: (IO [String]) -> (GK.Network -> GK.Operation [GK.NetworkPassword]) -> IO (Either Problem (String, String))
cliAccess getArgs find = do
    args <- getArgs
    findCreds $ cli args

    where

    findCreds :: (Either Problem String) -> IO (Either Problem (String, String))
    findCreds (Right dbname) = do
        answers <- GK.sync $ find $ dbloc dbname
        return $ check answers
        where

        check :: Either GK.KeyringError [GK.NetworkPassword] -> Either Problem (String, String)
        check (Right (c : _cs)) = checkUser $ GK.networkUser $ GK.networkPasswordNetwork c
            where
            checkUser (Just who) = Right (who, GK.networkPasswordSecret c)
            checkUser Nothing = Left NoUsername
        check (Right []) = Left $ NoMatchingPasswords dbname
        check (Left oops) = Left $ KeyRingError oops
    findCreds (Left oops) = return $ Left oops


cli :: [String] -> Either Problem String
cli [db] = Right db
cli _ = Left $ Usage "usage: prog dbname"

dbloc dbname = GK.network {
            GK.networkProtocol = Just "mysql",
            GK.networkServer = Just "localhost",
            GK.networkObject = Just dbname
        }

--	GLib.setApplicationName "GNOME Keyring example"
--	GLib.setProgramName "/path/to/binary"
