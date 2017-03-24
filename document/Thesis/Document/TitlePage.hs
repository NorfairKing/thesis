{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document.TitlePage
    ( titlePage
    ) where

import DocImport

import Data.ByteString (ByteString)

import Thesis.Document.Assets
import Thesis.Document.Types

logo :: Asset
logo = $(embedAsset "eth-logo.png")

titlePage :: Thesis
titlePage =
    titlepage $ do
        registerAsset logo
        includegraphics
            [KeepAspectRatio True, IGWidth $ CustomMeasure $ "0.5" <> textwidth]
            (assetPath logo)
        vfill
        center $ do
            huge $ "Signature Inference for Functional Property Discovery"
            raw "\\\\[\\baselineskip]"
            large $ "Tom Sydney Kerckhove"
        vfill
        flushright $
            tabular Nothing [LeftColumn, RightColumn] $ do
                "Advisor" & "Dr Dmitriy Trayel" <> lnbk
                "Supervisor" & "Prof. David Basin" <> lnbk
                "Department" & "Information Security" <> lnbk
                "Date" & "2017-09-09" <> lnbk
