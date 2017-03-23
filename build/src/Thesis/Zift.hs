module Thesis.Zift
    ( thesisZiftScript
    ) where

import Import

import Zifter

import Development.Shake
import Development.Shake.Path

import Thesis.Document
import Thesis.ShakeBuild

thesisZiftScript :: ZiftScript ()
thesisZiftScript = do
    preprocessor $ pure ()
    checker $
        liftIO $
        shakeArgs shakeOptions $ do
            thesisShakeBuildRules
            wantP [thesisOut]
