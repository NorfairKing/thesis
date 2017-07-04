{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.EntirePresentation
    ( entirePresentation
    ) where

import PresImport

import Development.Shake
import System.Directory
import System.FilePath

import Thesis.Document.Assets

entirePresentation :: Thesis
entirePresentation = do
    documentclass [] beamer
    -- No nav symbols
    comm0 "beamertemplatenavigationsymbolsempty"
    -- Color theme
    withRegisteredAsset solarizedtheme $
        const $ usecolortheme (raw "accent=yellow") (raw "solarized")
    -- Basic info
    title "Signature Inference for Functional Property Discovery"
    subtitle $ raw "or: How never to write tests manually anymore(*)"
    date $ raw "27 July 2017"
    author "Tom Sydney Kerckhove"
    institute Nothing $ do
        "ETH Zurich"
        lnbk
        url "https://cs-syd.eu/"
        lnbk
        url "https://github.com/NorfairKing"
    -- The presentation
    document $ do
        maketitle
        f "Me" $ do
            "I have a vague idea about what I am talking about."
            lnbk
            "Student at ETH"
            lnbk
            "Previously at Facebook, writing Haskell"
            lnbk
            "Teaching Assistant at ETH, teaching Haskell"
            lnbk
            "Research Assistant at ETH, writing Haskell"
            lnbk
            "Consultant at CS Kerckhove, consulting in Haskell"
            lnbk
            "Looking for a job!"
            lnbk
        motivation

motivation :: Thesis
motivation = do
    let d :: Thesis -> Thesis -> Thesis
        d a t = do
            pause
            a <> ": "
            t
            lnbk
    let user = d "user"
    let mach = d "machine"
    let math = d "maths"
    section "Motivation" $ do
        f "Motivation" $ center "Writing correct software is hard for humans."
        lightbulbslide
        f "Motivation" $ center "Make machines do it!"
        comment
            "I like to imagine the process of software development as a dialogue between me and a very picky and very literal person"
        f "Making machines write correct code" $ do
            user "I want a function to sort stuff"
            mach "What does 'sort' mean?"
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
        f "Making machines test that my code works" $ do
            insertGif $(embedAsset "typing.gif")

insertGif :: Asset -> Thesis
insertGif asset = do
    let rap = assetPath asset
    let pngp = rap -<.> "png"
    t $
        registerAction (assetPath asset) $ \rootdir -> do
            rd <- resolveDir' rootdir
            makeAsset rd asset
            cmd
                (Cwd rootdir)
                ("convert" :: String)
                ("-coalesce" :: String)
                rap
                pngp :: IO ()
    packageDep_ "animate"
    let prefix = dropExtensions pngp <> "-"
    n <-
        liftIO $ do
            cd <- getCurrentDirectory
            fs <- listDirectory (takeDirectory rap)
            let fs' = catMaybes $ map (stripPrefix cd) fs
            pure $ length $ filter (isPrefixOf prefix) fs'
    raw $
        mconcat
            [ "\\animategraphics[loop,controls,width=\\linewidth]{12}{"
            , fromString prefix
            , "}{0}{"
            , fromString (show n)
            , "}"
            ]

lightbulbslide :: Thesis
lightbulbslide =
    f "Idea" $
    figure (Just Center) $ do
        withRegisteredAsset $(embedAsset "lightbulb.png") $ \fp ->
            includegraphics
                [ KeepAspectRatio True
                , IGWidth $ CustomMeasure $ "0.3" <> textwidth
                ]
                fp
