-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Main (main) where

import System.Environment (getArgs)
import qualified Data.Text.Lazy as Text

import Davl.Domain (Party(..))
import Davl.UI (interactiveMain)

main :: IO ()
main = do
    args <- getArgs
    case parseArgs args of
        Just party -> interactiveMain party
        Nothing -> do
            putStrLn $ "failed to parse command line: " <> show args
            interactiveMain defaultParty

parseArgs :: [String] -> Maybe Party
parseArgs = \case
    [who] -> Just (Party (Text.pack who))
    [] -> Just defaultParty
    _ -> Nothing

defaultParty :: Party
defaultParty = Party "Alice"