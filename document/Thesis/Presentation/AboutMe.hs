{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.AboutMe
    ( aboutme
    ) where

import PresImport

aboutme :: Thesis
aboutme =
    f "About Me" $ do
        "Student at ETH"
        lnbk
        "This is my master thesis"
        lnbk
        "Wrote Haskell in open source"
        lnbk
        "Taught Haskell at ETH"
        lnbk
        "Wrote Haskell in industry"
        lnbk
        "Looking for a job!"
        lnbk
        vfill
        center $ do
            url "https://cs-syd.eu/"
            lnbk
            url "https://cs-syd.eu/cv"
            lnbk
            url "https://github.com/NorfairKing"
