{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.EntirePresentation
    ( entirePresentation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Presentation.Motivation
import Thesis.Presentation.PropertyDiscovery

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
        f "Me" $ do
            "I have a vague idea about what I am talking about."
            lnbk
            "Student at ETH"
            lnbk
            "Previously at Facebook, writing Haskell"
            lnbk
            "Teaching Assistant at ETH, teaching Haskell"
            lnbk
            "Research Assistant at ETH, writing Haskell"
            lnbk
            "Consultant at CS Kerckhove, consulting in Haskell"
            lnbk
            "Looking for a job!"
            lnbk
        motivation
        propertyDiscovery
