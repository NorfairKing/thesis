{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.EntirePresentation
    ( entirePresentation
    ) where

import PresImport

import Thesis.Document.Assets
import Thesis.Presentation.AboutMe
import Thesis.Presentation.Automation
import Thesis.Presentation.Motivation
import Thesis.Presentation.PropertyDiscovery
import Thesis.Presentation.SignatureInference
import Thesis.Presentation.Vision

entirePresentation :: Thesis
entirePresentation = do
    documentclass [] beamer
    -- No nav symbols
    comm0 "beamertemplatenavigationsymbolsempty"
    packageDep_ "pgfpages"
    bkind <- getBuildKind
    case bkind of
        BuildDraft -> do
            comm1 "setbeameroption" $ raw "show notes"
            comm1 "setbeameroption" $ raw "show notes on second screen=right"
        _ -> pure ()
    -- Color theme
    withRegisteredAsset solarizedtheme $
        const $ usecolortheme (raw "accent=yellow") (raw "solarized")
    mintedColors
    -- Basic info
    title "Signature Inference for Functional Property Discovery"
    subtitle $ raw "or: How never to come up with tests manually anymore(*)"
    date $ raw "27 July 2017"
    author "Tom Sydney Kerckhove"
    institute Nothing $ do
        "ETH Zurich"
        lnbk
        url "https://cs-syd.eu/"
        lnbk
        url "https://github.com/NorfairKing"
    -- The presentation
    document $ do
        maketitle
        note
            [ "The presentation should take about one hour."
            , "I have been working on this for the last four and a half months, so if I forget to explain anything, please ask me immediately."
            ]
        vision
        motivation
        propertyDiscovery
        automation
        signatureInference
        aboutme

mintedColors :: Thesis
mintedColors = do
    packageDep_ "xcolor"
    packageDep_ "minted"
    comm3 "definecolor" (raw "mintedbgcolor") (raw "rgb") (raw "0.95,0.95,0.95")
