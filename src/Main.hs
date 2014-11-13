module Main where

import qualified Gnome.Keyring as GK

main :: IO ()
main = do
  ok <- GK.available
  print ok
