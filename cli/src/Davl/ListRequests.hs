-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.ListRequests (
    ListAsBoss, listAsBoss,
    ListAsEmployee, listAsEmployee,
    ) where

import Data.List as List

import Davl.Domain
import Davl.Query

data ListAsBoss = ListAsBoss { list :: [Request] } -- *not* group by Employee
data ListAsEmployee = ListAsEmployee { list :: [Request] } -- *not* grouped by Boss

instance Show ListAsBoss where
    show ListAsBoss{list} =
        unlines ("As Boss:" : tab 2 (List.map f $ zip [1::Int ..] list))
        where f (i,r) = unwords ["#" <> show i, show r]

instance Show ListAsEmployee where
    show ListAsEmployee{list} =
        unlines ("As Employee:" : tab 2 (List.map f $ zip [1::Int ..] list))
        where f (i,r) = unwords ["#" <> show i, show r]

tab :: Int -> [String] -> [String]
tab n xs = List.map ((spaces <>)) xs where spaces = replicate n ' '

listAsBoss :: Party -> State -> ListAsBoss
listAsBoss whoami state = do
    let list = requestsAsBoss whoami state
    ListAsBoss {list}

listAsEmployee :: Party -> State -> ListAsEmployee
listAsEmployee whoami state = do
    let list = requestsAsEmployee whoami state
    ListAsEmployee {list}
