-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.Lazy as Text

import DA.Ledger (Party(..))
import Davl.HostAndPort(HostAndPort(..))
import Davl.UI (interactiveMain)

data Config = Config { host :: String, port :: Int, party :: Party }
    deriving (Show)

config0 :: Config
config0 = Config { host = "localhost", port = 6865, party = Party "Alice" }

main :: IO ()
main = do
    args <- getArgs
    let Config{host,port,party} = parseArgs config0 args
    let hp = HostAndPort{host,port}
    putStrLn $ "Connecting as " <> show party <> " to: " <> show hp
    interactiveMain hp party

parseArgs :: Config -> [String] -> Config
parseArgs config = \case
    "--host":host:args ->
        parseArgs (config { host }) args
    "--port":port:args ->
        parseArgs (config { port = read port }) args
    who:args ->
        parseArgs (config { party = Party (Text.pack who) }) args
    [] ->
        config


