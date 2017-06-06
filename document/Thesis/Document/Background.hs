{-# LANGUAGE OverloadedStrings #-}

module Thesis.Document.Background
    ( thesisBackground
    ) where

import DocImport

thesisBackground :: Thesis
thesisBackground =
    section "Background" $ do
        s "In this section I will introduce the setting of our work."
        subsection "QuickCheck" $ do
            s
                "Quickcheck was the first implementation of property testing and it is written in Haskell"
        subsection "QuickSpec" $ pure ()
