{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Vision
    ( vision
    ) where

import PresImport

vision :: Thesis
vision =
    section "Long term vision to wake the audience up" $ do
        f "Long term vision: A future in which ..." $
            center $ do
                "Software works"
                pause
                " because to test is cheaper than not to"
                pause
                ", even in the short term."
        f "Long term goal:" $ do
            center "We never come up with tests manually."
            comment
                "Spoiler: we're well on our way, and I'm going to show you a significant step in that direction."
