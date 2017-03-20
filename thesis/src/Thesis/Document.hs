{-# LANGUAGE TemplateHaskell #-}

module Thesis.Document where

import Import

import Development.Shake

import Thesis.LaTeX

documentRules :: Rules ()
documentRules = simpleLaTeXRules documentSpec

documentSpec :: LaTeXRulesSpec
documentSpec =
    LaTeXRulesSpec
    { latexTopDir = $(mkRelDir "document")
    , latexPdfOutFile = $(mkRelFile "thesis-tom-sydney-kerckhove.pdf")
    , latexMainTexFileName = $(mkRelFile "thesis")
    }
