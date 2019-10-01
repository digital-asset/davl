-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.ListDenials (
    ListAsEmployee, listAsEmployee, -- only for employees; boss's don't see denials
    ) where

import Data.List as List

import Davl.Domain
import Davl.Query

data ListAsEmployee = ListAsEmployee { list :: [Denial] } -- *not* grouped by Boss

instance Show ListAsEmployee where
    show ListAsEmployee{list} =
        unlines ("As Employee:" : tab 2 (List.map f $ zip [1::Int ..] list))
        where f (i,r) = unwords ["#" <> show i, show r]

tab :: Int -> [String] -> [String]
tab n xs = List.map ((spaces <>)) xs where spaces = replicate n ' '

listAsEmployee :: Party -> State -> ListAsEmployee
listAsEmployee whoami state = do
    let list = denialsAsEmployee whoami state
    ListAsEmployee {list}
