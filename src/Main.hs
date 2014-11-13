module Main where

import System.IO (stderr, hPrint)
import System.Exit (exitFailure)
import System.Environment (getArgs)
import qualified Gnome.Keyring as GK
import qualified Database.MySQL.Simple as DB

import DBAccess ( cliAccess, dbJob )


main :: IO ()
main = do
    maybeAccess <- cliAccess getArgs GK.findNetworkPassword DB.connect
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

--	GLib.setApplicationName "GNOME Keyring example"
--	GLib.setProgramName "/path/to/binary"
