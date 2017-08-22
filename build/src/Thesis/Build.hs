{-# LANGUAGE TemplateHaskell #-}

module Thesis.Build where

import Import

import System.Environment (withArgs)

import Text.LaTeX.LambdaTeX.Selection
import qualified Text.LaTeX.LambdaTeX.Selection.Types as Selection
import Text.LaTeX.LambdaTeX.Selection.Types (Selection)

import Development.Shake
import Development.Shake.Path

import Thesis.ShakeBuild

build :: Maybe String -> Maybe String -> Bool -> IO ()
build mtarget sel =
    buildWithThesisShake
        ((: []) $ fromMaybe "draft" mtarget)
        (fromMaybe [Selection.All] (constructSelection <$> sel))

buildFinal :: IO ()
buildFinal = buildWithThesisShake ["final"] [Selection.All] False

buildWithThesisShake :: [String] -> Selection -> Bool -> IO ()
buildWithThesisShake args sel f = do
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
        shakeArgs shakeOptions {shakeVerbosity = Loud, shakeVersion = version} $
        thesisShakeBuildRules sel f
