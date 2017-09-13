{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Public.Vision
    ( vision
    ) where

import PresImport

vision :: Thesis
vision =
    section "Long term vision" $ do
        f (raw "Long term vision: A future in which ...") $ do
            center $ do
                pause
                raw "Software works"
                pause
                raw " because is cheaper to make software that works"
                pause
                raw ", even in the short term."
            note
                [ s "I am not happy with the state of software today."
                , s "Maybe I'm just an annoying user, but I find that software very often doesn't work."
                , s "The reason, I think, is that it is often cheaper to make software that only sort of works, at least in the short term."
                ]
        f "Long term goal:" $ do
            center $ s "We never come up with tests manually."
            note
                [ s "Spoiler: we are well on our way, and I'm going to show you a significant step in that direction."
                ]
