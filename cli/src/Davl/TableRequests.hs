-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.TableRequests (
    TableAsBoss, tableAsBoss,
    TableAsEmployee, tableAsEmployee,
    ) where

import Data.List as List

import Davl.Domain
import Davl.ContractStore (State,activeContracts)

data TableAsBoss = TableAsBoss { list :: [Request] } -- *not* group by Employee
data TableAsEmployee = TableAsEmployee { list :: [Request] } -- *not* grouped by Boss

instance Show TableAsBoss where
    show TableAsBoss{list} =
        unlines ("As Boss:" : tab 2 (List.map f $ zip [1::Int ..] list))
        where f (i,r) = unwords ["#" <> show i, show r]

instance Show TableAsEmployee where
    show TableAsEmployee{list} =
        unlines ("As Employee:" : tab 2 (List.map f $ zip [1::Int ..] list))
        where f (i,r) = unwords ["#" <> show i, show r]

tab :: Int -> [String] -> [String]
tab n xs = List.map ((spaces <>)) xs where spaces = replicate n ' '


tableAsBoss :: Party -> State -> TableAsBoss
tableAsBoss whoami state = do
    let list = requestsAsBoss whoami state
    TableAsBoss {list}

tableAsEmployee :: Party -> State -> TableAsEmployee
tableAsEmployee whoami state = do
    let list = requestsAsEmployee whoami state
    TableAsEmployee {list}

requestsAsEmployee :: Party -> State -> [Request]
requestsAsEmployee whoami state = do
    x@Request{employee} <- requests state
    if (employee == whoami) then return x else []


-- TODO: move -> common/shared state queries

requestsAsBoss :: Party -> State -> [Request]
requestsAsBoss whoami state = do
    x@Request{boss} <- requests state
    if (boss == whoami) then return x else []

requests :: State -> [Request]
requests state = do
    (_,TRequest request) <- activeContracts state
    return request
