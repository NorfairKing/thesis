{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Motivation
    ( motivation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Presentation.Utils

motivation :: Thesis
motivation = do
    let d :: Thesis -> Thesis -> Thesis
        d a_ t_ = do
            pause
            a_ <> ": "
            t_
            lnbk
    let user = d "user"
    let mach = d "machine"
    let math = d "maths"
    section "Motivation" $ do
        f "Motivation" $ do
            pause
            center "Writing correct software is hard for humans."
            note
                [ "So why would we want to not want to come up with tests manually?"
                ]
        lightbulbslide
        note ["Here is an idea:"]
        f "Motivation" $ do
            center "Make machines do it!"
            note
                [ "It turns out that making machines write software is hard."
                , "I read on hacker news: One day we will only have to give the machine a precise description of what we want code to do, and the machine will write it for us."
                , "Well, we are already there. This precise description is called the code."
                ]
        lightbulbslide
        note
            [ "Alright, so maybe we cannot make machines write the code. New idea then."
            ]
        f "Motivation" $ do
            center
                "I will write the code myself, and get the machine to prove that it is correct."
            note
                [ "There are a few problems with this."
                , "First of all, you will run into Rice's theorem at some point."
                , "Second, you have to already know exactly what it means for your code to be correct."
                , "I argue that, in practice, formal methods will not solve the problem that writing correct code is expensive in the short term."
                ]
        lightbulbslide
        f "Motivation" $ do
            center
                "I will write the code myself, and get the machine to test that it works."
            note
                [ "When formal methods are too expensive, what do we turn to? Testing!"
                ]
        g "Making machines test that my code works" $ do
            only [FromSlide 2] $ do raw "\\setminted{highlightlines={2,4}}"
            hask $
                T.unlines
                    [ "sort"
                    , "    [4, 1, 6]"
                    , "        =="
                    , "            [1, 4, 6]"
                    ]
            pause
            vfill
            withRegisteredAsset $(embedAsset "code-coverage.png") $ \fp1 ->
                includegraphics
                    [ KeepAspectRatio True
                    , IGWidth $ CustomMeasure $ "0.64" <> textwidth
                    ]
                    fp1
            pause
            hfill
            withRegisteredAsset $(embedAsset "bitcoin-coin-single.pdf") $ \fp2 ->
                includegraphics
                    [ KeepAspectRatio True
                    , IGWidth $ CustomMeasure $ "0.30" <> textwidth
                    ]
                    fp2
        pictureSlide
            "Fixing the coverage problem"
            $(embedAsset "code-coverage.png")
        g "Property testing" $ do
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
            only [FromSlide 2] $
                center $
                withRegisteredAsset $(embedAsset "bitcoin-coin.pdf") $ \fp2 ->
                    includegraphics
                        [ KeepAspectRatio True
                        , IGWidth $ CustomMeasure $ "0.40" <> textwidth
                        ]
                        fp2
            pause
        pictureSlide "Fixing the cost problem" $(embedAsset "bitcoin-coin.pdf")
