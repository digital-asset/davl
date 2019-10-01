-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.ContractStore (
    State,
    initState, applyTrans, history,
    activeContracts,
    ) where

import Davl.Domain

data State = State { events :: [DavlEvent] } -- stored in most-recent-first order

initState :: State
initState = State []

history :: State -> [DavlEvent] -- returned in oldest-first order
history = reverse . events

applyTrans :: State -> DavlEvent -> State
applyTrans State{events} dc =
    State {events = dc : events }

archivedContractIds :: State -> [DavlContractId]
archivedContractIds state = do
    Archive id <- history state
    return id

activeContracts :: State -> [(DavlContractId,DavlTemplate)]
activeContracts state = do
    let archived = archivedContractIds state
    Create{id,info} <- history state
    if (id `notElem` archived) then return () else []
    return (id,info)
