{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Public.EntirePresentation
    ( entirePresentation
    ) where

import PresImport

import Thesis.Document.Assets
import Thesis.Presentation.Public.AboutMe
import Thesis.Presentation.Public.Automation
import Thesis.Presentation.Public.Motivation
import Thesis.Presentation.Public.PropertyDiscovery
import Thesis.Presentation.Public.SignatureInference

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
    date $ raw "11 October 2017"
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
