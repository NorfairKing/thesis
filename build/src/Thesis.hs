module Thesis where

import Import

import System.Environment (withArgs)

import Development.Shake
import Development.Shake.Path

import Thesis.Document
import Thesis.Document.Main
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    withArgs ["--color"] $
    shakeArgs shakeOptions {shakeVerbosity = Loud} $ do
        thesisShakeBuildRules
        wantP [thesisOut]
