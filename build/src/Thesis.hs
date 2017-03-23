{-# LANGUAGE TemplateHaskell #-}

module Thesis where

import Import

import System.Environment (withArgs)

import Development.Shake
import Development.Shake.Path

import Thesis.Document
import Thesis.ShakeBuild

thesis :: IO ()
thesis = do
    versionFiles <-
        snd <$>
        liftM2
            (<>)
            (listDirRecur $(mkRelDir "build"))
            (listDirRecur $(mkRelDir "document"))
    version <- getHashedShakeVersion $ map toFilePath versionFiles
    withArgs ["--color"] $
        shakeArgs shakeOptions {shakeVerbosity = Loud, shakeVersion = version} $ do
            thesisShakeBuildRules
            wantP [thesisOut]
