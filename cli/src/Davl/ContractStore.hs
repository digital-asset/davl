-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.ContractStore (
    State,
    initState, applyTrans, history,
    activeContracts,
    ) where

import Davl.Domain

data State = State { events :: [DavlEvent] } -- reverse order

initState :: State
initState = State []

history :: State -> [DavlEvent] -- forwards order
history = reverse . events

applyTrans :: Party -> State -> DavlEvent -> State
applyTrans _ State{events} dc =
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
