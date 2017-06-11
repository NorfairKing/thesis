{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background
    ( thesisBackground
    ) where

import DocImport

import Thesis.Document.Background.PropertyDiscovery
import Thesis.Document.Background.PropertyTesting

thesisBackground :: Thesis
thesisBackground =
    section "Background" $ do
        s "In this section I will introduce the setting of our work."
        s
            "A certain familiarity with the Haskell programming language is assumed."
        thesisBackgroundPropertyTesting
        thesisBackgroundPropertyDiscovery
