{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Vision
    ( vision
    ) where

import PresImport

vision :: Thesis
vision =
    section "Long term vision" $ do
        f "Long term vision: A future in which ..." $ do
            center $ do
                pause
                "Software works"
                pause
                " because is cheaper to make software that works"
                pause
                ", even in the short term."
            note
                [ "I am not happy with the state of software today."
                , "Maybe I'm just an annoying user, but I find that software very often doesn't work."
                , "The reason, I think, is that it is often cheaper to make software that only sort of works, at least in the short term."
                ]
        f "Long term goal:" $ do
            center "We never come up with tests manually."
            note
                [ "Spoiler: we are well on our way, and I'm going to show you a significant step in that direction."
                ]
