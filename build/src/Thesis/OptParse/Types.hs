module Thesis.OptParse.Types where

import Import

import Network.Mail.Mime

type Arguments = (Command, Flags)

type Instructions = (Dispatch, Settings)

data Command
    = CommandBuild (Maybe String)
                   (Maybe String)
                   (Maybe Bool)
    | CommandSendDraft SendFlags
    deriving (Show, Eq)

newtype SendFlags = SendFlags
    { sendFlagTo :: SendRecipientFlags
    } deriving (Show, Eq)

data SendRecipientFlags
    = SendRepByName SendRecipientByName
    | SendRepPreset String
    deriving (Show, Eq)

data SendRecipientByName = SendRecipientByName
    { sendFlagToName :: Maybe String
    , sendFlagToMail :: String
    } deriving (Show, Eq)

data Flags =
    Flags
    deriving (Show, Eq)

data Configuration =
    Configuration
    deriving (Show, Eq)

data Dispatch
    = DispatchBuild (Maybe String)
                    (Maybe String)
                    Bool
    | DispatchSendDraft SendArgs
    deriving (Show, Eq)

newtype SendArgs = SendArgs
    { sendTo :: SendRecipient
    } deriving (Show, Eq)

data SendRecipient
    = ToPreset String
    | ToAddress Address
    deriving (Show, Eq)

data Settings =
    Settings
    deriving (Show, Eq)
