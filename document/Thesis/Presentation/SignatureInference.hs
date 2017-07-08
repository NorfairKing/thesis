{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Thesis.Presentation.SignatureInference
    ( signatureInference
    ) where

import PresImport

import qualified Data.Text as T

import Thesis.Document.Assets
import Thesis.Document.Dependencies

import Thesis.Presentation.Motivation

signatureInference :: Thesis
signatureInference =
    section "Signature Inference" $ do
        f "" $ center "Automated, but still slow"
        lightbulbslide
        f "Critical insight" $
            center "We are not interested in the entire codebase."
