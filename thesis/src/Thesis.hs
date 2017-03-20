module Thesis where

import Import

import Development.Shake

import Thesis.Document
import Thesis.LaTeX
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions {shakeVerbosity = Loud} $ do
        thesisShakeBuildRules
        wantLaTeX documentSpec
