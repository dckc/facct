{-# LANGUAGE Trustworthy #-}

module Tamed.Keyring (
    Network, NetworkPassword, Operation, KeyringError,
    findNetworkPassword,
    network, networkUser, networkProtocol, networkServer, networkObject,
    networkPasswordNetwork, networkPasswordSecret)
 where

import Gnome.Keyring (
    Network, NetworkPassword, Operation, KeyringError,
    findNetworkPassword,
    network, networkUser, networkProtocol, networkServer, networkObject,
    networkPasswordNetwork, networkPasswordSecret)
