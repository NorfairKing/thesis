{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background
    ( thesisBackground
    ) where

import DocImport

import Thesis.Document.Background.Haskell
import Thesis.Document.Background.PropertyTesting

thesisBackground :: Thesis
thesisBackground =
    section "Background" $ do
        s "In this section I will introduce the setting of our work."
        thesisBackgroundHaskell
        thesisBackgroundPropertyTesting
