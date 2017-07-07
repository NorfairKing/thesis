{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.EntirePresentation
    ( entirePresentation
    ) where

import PresImport

import Thesis.Document.Assets
import Thesis.Presentation.AboutMe
import Thesis.Presentation.Motivation
import Thesis.Presentation.PropertyDiscovery
import Thesis.Presentation.Vision

entirePresentation :: Thesis
entirePresentation = do
    documentclass [] beamer
    -- No nav symbols
    comm0 "beamertemplatenavigationsymbolsempty"
    -- Color theme
    withRegisteredAsset solarizedtheme $
        const $ usecolortheme (raw "accent=yellow") (raw "solarized")
    -- Basic info
    title "Signature Inference for Functional Property Discovery"
    subtitle $ raw "or: How never to write tests manually anymore(*)"
    date $ raw "27 July 2017"
    author "Tom Sydney Kerckhove"
    institute Nothing $ do
        "ETH Zurich"
        lnbk
        url "https://cs-syd.eu/"
        lnbk
        url "https://github.com/NorfairKing"
    -- Set template
    raw "\\beamertemplatenavigationsymbolsempty"
    raw "\\setbeamertemplate{footline}[frame number]"
    raw "\\setbeamerfont{page number in head/foot}{size=\\normalsize}"
    raw "\\setbeamercolor{page number in head/foot}{fg=black}"
    -- The presentation
    document $ do
        maketitle
        vision
        aboutme
        motivation
        propertyDiscovery
