module Thesis.OptParse
    ( getInstructions
    , Dispatch(..)
    , Settings(..)
    , SendArgs(..)
    , SendRecipient(..)
    ) where

import Import

import qualified Data.Text as T
import Network.Mail.Mime
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
            CommandBuild as sel b -> DispatchBuild as sel $ fromMaybe False b
            CommandSendDraft sfs ->
                DispatchSendDraft
                    SendArgs
                    { sendTo =
                          case sendFlagTo sfs of
                              SendRepByName srbn ->
                                  ToAddress
                                      Address
                                      { addressName =
                                            T.pack <$> sendFlagToName srbn
                                      , addressEmail =
                                            T.pack $ sendFlagToMail srbn
                                      }
                              SendRepPreset preset -> ToPreset preset
                    }
            CommandBuildFinal -> DispatchBuildFinal

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
        , command "build-final" parseCommandBuildFinal
        ]

parseCommandBuild :: ParserInfo Command
parseCommandBuild = info parser modifier
  where
    parser =
        CommandBuild <$>
        argument
            (Just <$> str)
            (mconcat
                 [metavar "TARGET", value Nothing, help "the targets to build"]) <*>
        option
            (Just <$> str)
            (mconcat
                 [ long "selection"
                 , metavar "SELECTION"
                 , value Nothing
                 , help "the selection of parts to build"
                 ]) <*>
        option
            (Just <$> auto)
            (mconcat
                 [ long "fast"
                 , metavar "BOOL"
                 , value Nothing
                 , help "build the document faster."
                 ])
    modifier = fullDesc <> progDesc "Build a draft document"

parseCommandSendDraft :: ParserInfo Command
parseCommandSendDraft = info parser modifier
  where
    parser = CommandSendDraft <$> parseSendFlags
    modifier =
        fullDesc <> progDesc "Build a draft document and send it via email."

parseCommandBuildFinal :: ParserInfo Command
parseCommandBuildFinal = info parser modifier
  where
    parser = pure CommandBuildFinal
    modifier = fullDesc <> progDesc "Build the final thesis document."

parseSendFlags :: Parser SendFlags
parseSendFlags =
    fmap SendFlags $
    subparser $
    mconcat
        [ command "by-name" $
          info
              (fmap SendRepByName $
               SendRecipientByName <$>
               argument
                   (Just <$> str)
                   (mconcat [metavar "NAME", help "The name of the recipient"]) <*>
               strArgument
                   (mconcat
                        [ metavar "EMAIL"
                        , help "The email address of the recipient"
                        ]))
              briefDesc
        , command "by-preset" $
          info
              (SendRepPreset <$>
               strArgument
                   (mconcat
                        [help "The name of the preset to use for the recipient"]))
              briefDesc
        ]

parseFlags :: Parser Flags
parseFlags = pure Flags
