module Thesis.OptParse
    ( module Thesis.OptParse
    , module Thesis.OptParse.Types
    ) where

import Import

import System.Environment (getArgs)

import Options.Applicative

import Thesis.OptParse.Types

getInstructions :: IO Instructions
getInstructions = do
    (cmd, flags) <- getArguments
    config <- getConfiguration cmd flags
    combineToInstructions cmd flags config

combineToInstructions :: Command -> Flags -> Configuration -> IO Instructions
combineToInstructions cmd Flags Configuration = pure (disp, Settings)
  where
    disp =
        case cmd of
            CommandBuild -> DispatchBuild
            CommandSendDraft -> DispatchSendDraft

getConfiguration :: Command -> Flags -> IO Configuration
getConfiguration _ _ = pure Configuration

getArguments :: IO Arguments
getArguments = do
    args <- getArgs
    let result = runArgumentsParser args
    handleParseResult result

runArgumentsParser :: [String] -> ParserResult Arguments
runArgumentsParser = execParserPure prefs_ argParser
  where
    prefs_ =
        ParserPrefs
        { prefMultiSuffix = ""
        , prefDisambiguate = True
        , prefShowHelpOnError = True
        , prefShowHelpOnEmpty = True
        , prefBacktrack = True
        , prefColumns = 80
        }

argParser :: ParserInfo Arguments
argParser = info (helper <*> parseArgs) help_
  where
    help_ = fullDesc <> progDesc description
    description = "Thesis"

parseArgs :: Parser Arguments
parseArgs = (,) <$> parseCommand <*> parseFlags

parseCommand :: Parser Command
parseCommand =
    hsubparser $
    mconcat
        [ command "build" parseCommandBuild
        , command "send-draft" parseCommandSendDraft
        ]

parseCommandBuild :: ParserInfo Command
parseCommandBuild = info parser modifier
  where
    parser = pure CommandBuild
    modifier = fullDesc <> progDesc "Build a draft document"

parseCommandSendDraft :: ParserInfo Command
parseCommandSendDraft = info parser modifier
  where
    parser = pure CommandSendDraft
    modifier =
        fullDesc <> progDesc "Build a draft document and send it via email."

parseFlags :: Parser Flags
parseFlags = pure Flags
