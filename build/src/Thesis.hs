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
dispatch (DispatchBuild mtarget sel f) = build mtarget sel f
dispatch (DispatchSendDraft sas) = sendDraft sas
dispatch DispatchBuildFinal = buildFinal
