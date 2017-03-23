{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.TitlePage
    ( titlePage
    ) where

import DocImport

import Thesis.Document.Types

titlePage :: Thesis
titlePage =
    titlepage $ do
        includegraphics
            [KeepAspectRatio True, IGWidth $ CustomMeasure $ "0.5" <> textwidth]
            "../assets/eth-logo.png"
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
