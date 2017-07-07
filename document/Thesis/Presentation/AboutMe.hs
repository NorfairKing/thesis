{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.AboutMe
    ( aboutme
    ) where

import PresImport

aboutme :: Thesis
aboutme =
    f "About Me" $ do
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
