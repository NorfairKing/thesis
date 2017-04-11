{-# LANGUAGE TemplateHaskell #-}

module Thesis.Build where

import Import

import System.Environment (withArgs)

import Development.Shake
import Development.Shake.Path

import Thesis.ShakeBuild

build :: Maybe String -> IO ()
build mtarget =
    buildWithThesisShake $
    case mtarget of
        Nothing -> ["draft"]
        Just target -> [target]

buildWithThesisShake :: [String] -> IO ()
buildWithThesisShake args = do
    versionFiles <-
        snd <$>
        liftM2
            (<>)
            (fromMaybe ([], []) <$>
             forgivingAbsence (listDirRecur $(mkRelDir "build")))
            (fromMaybe ([], []) <$>
             forgivingAbsence (listDirRecur $(mkRelDir "document")))
    version <- getHashedShakeVersionP versionFiles
    withArgs (args ++ ["--color"]) $
        shakeArgs
            shakeOptions {shakeVerbosity = Loud, shakeVersion = version}
            thesisShakeBuildRules
