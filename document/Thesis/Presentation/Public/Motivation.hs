{-# LANGUAGE OverloadedStrings #-}

module Thesis.Presentation.Public.Motivation
    ( motivation
    ) where

import PresImport

motivation :: Thesis
motivation =
    section "Motivation" $ do
        f "Motivation" $
            center $ s "Writing correct software is hard for humans."
        g "Unit Testing" $ do
            only [FromSlide 2] $ raw "\\setminted{highlightlines={2,4}}"
            haskL
                ["sort", "    [4, 1, 6]", "        ==", "            [1, 4, 6]"]
        g "Property testing" $ do
            only [OneSlide 1] $ raw "\\setminted{highlightlines=1}"
            only [OneSlide 2] $ raw "\\setminted{highlightlines=2}"
            only [OneSlide 3] $ raw "\\setminted{highlightlines=4}"
            haskL
                [ "  forAll"
                , "    arbitrary"
                , "      $ \\ls ->"
                , "        isSorted (sort ls)"
                ]
