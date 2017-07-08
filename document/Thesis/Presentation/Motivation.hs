{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.Motivation
    ( motivation
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets

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
        f "Motivation" $ center "Writing correct software is hard for humans."
        comment "Bear with me, this part is relevant!"
        lightbulbslide
        f "Motivation" $ center "Make machines do it!"
        comment
            "I like to imagine the process of software development as a dialogue between me and a very picky and very literal person"
        f "Making machines write correct code" $ do
            user "I want a function to sort stuff"
            mach "What does it mean to sort?"
            mach "What stuff?"
            mach "What is a function?"
            user "Oh dear, never mind, I will just do it myself."
        lightbulbslide
        f "Motivation" $
            center
                "I will write the code myself, and get the machine to prove that it is correct."
        f "Making machines check if my code is correct" $ do
            user "Let's see, I have code to sort stuff."
            user "I know what it means for this code to be correct."
            user "Now I just write code that proves that this code is correct."
            math "That will not work."
            user "Why is that?"
            math "You cannot use code to prove things about code.*"
            user "Oh, never?"
            math "Well, sometimes, but good luck with that!"
        comment "The point is that formal methods are expensive."
        lightbulbslide
        f "Motivation" $
            center
                "I will write the code myself, and get the machine to test that it works."
        g "Making machines test that my code works" $ do
            only [FromSlide 1] $ do raw "\\setminted{highlightlines={2,4}}"
            hask $
                T.unlines
                    [" sort", "  [4, 1, 6]", "   `shouldBe`", "     [1, 4, 6]"]
            vfill
            withRegisteredAsset $(embedAsset "code-coverage.png") $ \fp1 ->
                includegraphics
                    [ KeepAspectRatio True
                    , IGWidth $ CustomMeasure $ "0.64" <> textwidth
                    ]
                    fp1
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
            only [OneSlide 3] $ raw "\\setminted{highlightlines=6}"
            hask $
                T.unlines
                    [ "  forAll"
                    , "    arbitrary"
                    , "      $ \\ls ->"
                    , "        sort ls"
                    , "          `shouldSatisfy`"
                    , "            isSorted"
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
