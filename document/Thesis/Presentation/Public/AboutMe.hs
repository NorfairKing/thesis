{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Public.AboutMe
    ( aboutme
    ) where

import PresImport

aboutme :: Thesis
aboutme =
    f "About Me" $ do
        "This was my master thesis"
        lnbk
        "Wrote Haskell in open source"
        lnbk
        "Taught Haskell at ETH"
        lnbk
        "Haskell and DevOps in industry"
        vfill
        center $ do
            url "https://cs-syd.eu/"
            lnbk
            url "https://cs-syd.eu/cv"
            lnbk
            url "https://github.com/NorfairKing"
