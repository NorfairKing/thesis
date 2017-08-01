{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Thesis.SendDraft where

import Import

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Text.LaTeX.LambdaTeX.Selection.Types as Selection

import Development.GitRev
import Network.Mail.Mime

import Thesis.Build
import Thesis.OptParse

sendDraft :: SendArgs -> IO ()
sendDraft SendArgs {..} = do
    buildWithThesisShake ["draft"] [Selection.All] False
    let from =
            Address
            { addressName = Just "Tom Sydney Kerckhove"
            , addressEmail = "syd.kerckhove@gmail.com"
            }
    to <-
        case sendTo of
            ToAddress addr -> pure addr
            ToPreset preset ->
                case lookup preset presets of
                    Nothing ->
                        die $ unwords ["No preset found with name", preset]
                    Just a -> pure a
    let clearMail =
            Mail
            { mailFrom = from
            , mailTo = [to]
            , mailCc = []
            , mailBcc = []
            , mailHeaders =
                  [("Subject", T.unwords ["Thesis draft", T.pack $(gitHash)])]
            , mailParts = [[plainPart $ makeMailContent from to]]
            }
    mail <- addAttachment "application/pdf" "out/draft.pdf" clearMail
    renderSendMail mail

makeMailContent :: Address -> Address -> LT.Text
makeMailContent from to =
    LT.fromStrict $
    T.unlines
        [ T.unwords ["Dear", fromMaybe (addressEmail to) (addressName to)]
        , ""
        , "Please find a draft of my thesis attached to this email."
        , T.unwords ["This is version", T.pack $(gitHash)]
        , ""
        , "Thank you for your time."
        , ""
        , fromMaybe (addressEmail from) (addressName from)
        , ""
        , "---"
        , "This draft was sent automatically."
        ]

presets :: [(String, Address)]
presets =
    [ ( "nick"
      , Address
        { addressName = Just "Nick Van den Broeck"
        , addressEmail = "nick.van.den.broeck@hotmail.com"
        })
    , ( "xavier"
      , Address
        { addressName = Just "Xavier Goás Aguililla"
        , addressEmail = "xavier.goas@gmail.com"
        })
    , ( "dmitriy"
      , Address
        { addressName = Just "Dmitriy Traytel"
        , addressEmail = "traytel@inf.ethz.ch"
        })
    , ( "alix"
      , Address
        { addressName = Just "Alix Robinson"
        , addressEmail = "juicesteps@gmail.com"
        })
    ]
