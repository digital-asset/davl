-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Davl.Logging (Logger,noLog,tagLog,colourLog,colourWrap) where

import System.Console.ANSI (
    setSGRCode, Color(..), SGR(SetColor), ConsoleLayer(Foreground), ColorIntensity(Vivid),
    )

type Logger = String -> IO ()

noLog :: Logger
noLog _ = return ()

colourWrap :: Color -> String -> String
colourWrap col s =
  setSGRCode [SetColor Foreground Vivid col] <> s <>
  setSGRCode [SetColor Foreground Vivid White]

colourLog :: Color -> Logger -> Logger
colourLog col log s = log (colourWrap col s)

tagLog :: String -> Logger -> Logger
tagLog tag log s = log (tag <> s)
