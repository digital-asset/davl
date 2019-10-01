-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Domain (

    Party(..),
    Holiday(..),
    Gift(..),
    Request(..),
    Date(..),
    Denial(..),

    DavlContractId(..),
    DavlTemplate(..),
    DavlEvent(..),
    DavlCommand(..)

    ) where

import qualified Data.Text.Lazy as Text(unpack)

import DA.Ledger.Types (Party(..),ContractId(..))

data Holiday = Holiday { employee :: Party, boss :: Party } deriving (Show)

data Gift = Gift { allocation :: Holiday } deriving (Show)

data Request = Request
    { employee :: Party
    , boss :: Party
    , allocationId :: DavlContractId
    , date :: Date
    } deriving (Show)

data Date = Date { daysSinceEpoch :: Int } deriving (Show) -- TODO: use a standard type

data Denial = Denial
    { employee :: Party
    , boss :: Party
    , allocationId :: DavlContractId
    , date :: Date
    , reason :: String
    } deriving (Show)

data DavlTemplate
    = TGift Gift
    | THoliday Holiday
    | TRequest Request
    | TDenial Denial

newtype DavlContractId = DavlContractId { cid :: ContractId } deriving (Eq)

data DavlEvent
    = Create { id :: DavlContractId, info :: DavlTemplate }
    | Archive DavlContractId

data DavlCommand
    = GiveGift Gift
    | ClaimGift DavlContractId
    | RequestHoliday Request
    | DenyRequest DavlContractId String

instance Show DavlTemplate where
    show = \case
        TGift x -> show x
        THoliday x -> show x
        TRequest x -> show x
        TDenial x -> show x

instance Show DavlEvent where
    show = \case
        Create {id,info} -> "CREATE: " <> show id <> " = " <> show info
        Archive id -> "ARCHIVE: " <> show id

instance Show DavlContractId where
    show DavlContractId{cid} = "[" <> Text.unpack (unContractId cid) <> "]"
