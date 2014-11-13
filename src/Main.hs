module Main where

import System.IO
import System.Exit
import qualified System.Environment as Sys
import qualified Gnome.Keyring as GK

main :: IO ()
main = do
    args <- Sys.getArgs
    run $ cli args
    where
        run (Right dbname) = do
            creds <- findCreds GK.findNetworkPassword dbname
            explore creds
            where
                explore (Right (c : _cs)) = print (GK.networkUser $ GK.networkPasswordNetwork c,
                                                    GK.networkPasswordSecret c)
                explore (Right []) = lose "no matching passwords"
                explore (Left oops) = lose oops
        run (Left oops) = lose oops
        lose oops = do
            hPrint stderr oops
            exitFailure


cli :: [String] -> Either String String
cli [db] = Right db
cli _ = Left "usage: prog dbname"


findCreds find db = GK.sync $ find loc
    where loc = GK.network {
     GK.networkProtocol = Just "mysql",
     GK.networkServer = Just "localhost",
     GK.networkObject = Just db
     }


--	GLib.setApplicationName "GNOME Keyring example"
--	GLib.setProgramName "/path/to/binary"
