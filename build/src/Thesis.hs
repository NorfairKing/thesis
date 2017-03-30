module Thesis where

import Import

import Thesis.Build
import Thesis.OptParse
import Thesis.SendDraft

thesis :: IO ()
thesis = do
    (disp, Settings) <- getInstructions
    dispatch disp

dispatch :: Dispatch -> IO ()
dispatch DispatchBuild = build
dispatch (DispatchSendDraft sas) = sendDraft sas
