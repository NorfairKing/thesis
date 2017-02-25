module Thesis where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions {shakeVerbosity = Loud} $ do
        thesisShakeBuildRules
        wantP []
