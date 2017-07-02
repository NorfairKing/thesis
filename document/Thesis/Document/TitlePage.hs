{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document.TitlePage
    ( titlePage
    ) where

import DocImport

import Thesis.Document.Assets

logo :: Asset
logo = $(embedAsset "eth-logo.png")

titlePage :: Thesis
titlePage =
    titlepage $ do
        withRegisteredAsset logo $ \logoPath ->
            includegraphics
                [ KeepAspectRatio True
                , IGWidth $ CustomMeasure $ "0.5" <> textwidth
                ]
                logoPath
        vfill
        center $ do
            large "Master thesis"
            raw "\\\\[\\baselineskip]"
            huge "Signature Inference for Functional Property Discovery"
            raw "\\\\[\\baselineskip]"
            large "Tom Sydney Kerckhove"
        vfill
        flushright $
            tabular Nothing [LeftColumn, RightColumn] $ do
                "Adviser" & "Dr Dmitriy Traytel" <> lnbk
                "Supervisor" & "Prof. David Basin" <> lnbk
                "Department" & "Information Security" <> lnbk
                "Date" & raw "2017-09-09" <> lnbk
