module Thesis where

import Import

import System.Environment (withArgs)

import Development.Shake

import Thesis.Document
import Thesis.Document.Main
import Thesis.LaTeX
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    if False
        then
            withArgs ["--color"] $
                shakeArgs shakeOptions {shakeVerbosity = Loud} $ do
                    thesisShakeBuildRules
                    wantLaTeX documentSpec
        else buildThesisDocument
