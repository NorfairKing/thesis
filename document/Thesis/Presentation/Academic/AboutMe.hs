{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Academic.AboutMe
    ( aboutme
    ) where

import PresImport

aboutme :: Thesis
aboutme =
    f "About Me" $ do
        "Student at ETH, " <> raw "Z\\\"urich"
        lnbk
        "This is my master thesis"
        lnbk
        vfill
        center $ do
            url "https://cs-syd.eu/"
            lnbk
            url "https://cs-syd.eu/cv"
            lnbk
            url "https://github.com/NorfairKing"
