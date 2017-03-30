{-# LANGUAGE TemplateHaskell #-}

module Thesis.Build where

import Import

import System.Environment (withArgs)

import Development.Shake
import Development.Shake.Path

import Thesis.Document
import Thesis.ShakeBuild

build :: IO ()
build = do
    versionFiles <-
        snd <$>
        liftM2
            (<>)
            (fromMaybe ([], []) <$>
             forgivingAbsence (listDirRecur $(mkRelDir "build")))
            (fromMaybe ([], []) <$>
             forgivingAbsence (listDirRecur $(mkRelDir "document")))
    version <- getHashedShakeVersion $ map toFilePath versionFiles
    withArgs ["--color"] $
        shakeArgs shakeOptions {shakeVerbosity = Loud, shakeVersion = version} $ do
            thesisShakeBuildRules
            wantP [thesisOut]
