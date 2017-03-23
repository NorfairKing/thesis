module Thesis where

import Import

import System.Environment (withArgs)

import Development.Shake

import Thesis.Document
import Thesis.LaTeX
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    withArgs ["--color"] $
    shakeArgs shakeOptions {shakeVerbosity = Loud} $ do
        thesisShakeBuildRules
        wantLaTeX documentSpec
