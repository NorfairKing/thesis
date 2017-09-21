{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Academic.Motivation
    ( motivation
    ) where

import PresImport

import qualified Data.Text as T

motivation :: Thesis
motivation =
    section "Motivation" $ do
        f "Motivation" $ do
            center $ s "Writing correct software is hard for humans."
            note
                [ "So why would we want to not want to come up with tests manually?"
                ]
        g "Unit Testing" $ do
            only [FromSlide 2] $ raw "\\setminted{highlightlines={2,4}}"
            hask $
                T.unlines
                    [ "sort"
                    , "    [4, 1, 6]"
                    , "        =="
                    , "            [1, 4, 6]"
                    ]
        g "Property Testing" $ do
            only [OneSlide 1] $ raw "\\setminted{highlightlines=1}"
            only [OneSlide 2] $ raw "\\setminted{highlightlines=2}"
            only [OneSlide 3] $ raw "\\setminted{highlightlines=4}"
            hask $
                T.unlines
                    [ "  forAll"
                    , "    arbitrary"
                    , "      $ \\ls ->"
                    , "        isSorted (sort ls)"
                    ]
            pause
