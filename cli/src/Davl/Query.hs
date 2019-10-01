-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Query (
    State, -- reexport from ContractStore
    giftsAsEmployee, giftsAsBoss,
    holidaysAsEmployee, holidaysAsBoss,
    requestsAsEmployee, requestsAsBoss,
    denialsAsEmployee,
    vacationsAsEmployee, vacationsAsBoss,
    ) where

import Davl.Domain
import Davl.ContractStore (State,activeContracts)


giftsAsBoss :: Party -> State -> [Gift]
giftsAsBoss whoami state = do
    g@Gift{allocation=Holiday{boss}} <- gifts state
    if (boss == whoami) then return g else []

giftsAsEmployee :: Party -> State -> [Gift]
giftsAsEmployee whoami state = do
    g@Gift{allocation=Holiday{employee}} <- gifts state
    if (employee == whoami) then return g else []


holidaysAsBoss :: Party -> State -> [Holiday]
holidaysAsBoss whoami state = do
    g@Holiday{boss} <- holidays state
    if (boss == whoami) then return g else []

holidaysAsEmployee :: Party -> State -> [Holiday]
holidaysAsEmployee whoami state = do
    g@Holiday{employee} <- holidays state
    if (employee == whoami) then return g else []


requestsAsEmployee :: Party -> State -> [Request]
requestsAsEmployee whoami state = do
    x@Request{employee} <- requests state
    if (employee == whoami) then return x else []

requestsAsBoss :: Party -> State -> [Request]
requestsAsBoss whoami state = do
    x@Request{boss} <- requests state
    if (boss == whoami) then return x else []


denialsAsEmployee :: Party -> State -> [Denial]
denialsAsEmployee whoami state = do
    x@Denial{employee} <- denials state
    if (employee == whoami) then return x else []

--denialsAsBoss :: Party -> State -> [Denial] -- (currently the model does not allow bosses to see their denial)


vacationsAsEmployee :: Party -> State -> [Vacation]
vacationsAsEmployee whoami state = do
    x@Vacation{employee} <- vacations state
    if (employee == whoami) then return x else []

vacationsAsBoss :: Party -> State -> [Vacation]
vacationsAsBoss whoami state = do
    x@Vacation{boss} <- vacations state
    if (boss == whoami) then return x else []



gifts :: State -> [Gift]
gifts state = do
    (_,TGift gift) <- activeContracts state
    return gift

holidays :: State -> [Holiday]
holidays state = do
    (_,THoliday holiday) <- activeContracts state
    return holiday

requests :: State -> [Request]
requests state = do
    (_,TRequest request) <- activeContracts state
    return request

denials :: State -> [Denial]
denials state = do
    (_,TDenial denial) <- activeContracts state
    return denial

vacations :: State -> [Vacation]
vacations state = do
    (_,TVacation vacation) <- activeContracts state
    return vacation
