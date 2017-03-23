module Thesis.Zift
    ( thesisZiftScript
    ) where

import Import

import Zifter

import Development.Shake

import Thesis.Document
import Thesis.LaTeX
import Thesis.ShakeBuild

thesisZiftScript :: ZiftScript ()
thesisZiftScript = do
    preprocessor $ pure ()
    checker $
        liftIO $
        shakeArgs shakeOptions $ do
            thesisShakeBuildRules
            wantLaTeX documentSpec
