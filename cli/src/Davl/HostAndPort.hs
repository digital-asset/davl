-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.HostAndPort (
    HostAndPort(..),
    toLedgerConfig
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified DA.Ledger as Ledger

data HostAndPort = HostAndPort { host :: String, port :: Int }

instance Show HostAndPort where
    show HostAndPort{host,port} = host <> ":" <> show port

toLedgerConfig :: HostAndPort -> Ledger.ClientConfig
toLedgerConfig HostAndPort{host,port} =
    Ledger.configOfHostAndPort (Ledger.Host (BS.pack host)) (Ledger.Port port)
