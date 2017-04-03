module Thesis.Zift
    ( thesisZiftScript
    ) where

import Import

import System.Environment (withArgs)

import Development.Shake
import Development.Shake.Path

import Zifter

import Thesis.Document
import Thesis.ShakeBuild

thesisZiftScript :: ZiftScript ()
thesisZiftScript = do
    preprocessor $ pure ()
    checker $
        liftIO $
        withArgs [] $
        shakeArgs shakeOptions $ do
            thesisShakeBuildRules
            wantP [thesisOut]
