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
    , latexMainTexFileName = $(mkRelFile "thesis.tex")
    , latexRunConfigs =
          [ LaTeXRunConfig
            { latexPdfOutFile = $(mkRelFile "final.pdf")
            , latexInternalFlags = ["FINAL"]
            }
          , LaTeXRunConfig
            { latexPdfOutFile = $(mkRelFile "draft.pdf")
            , latexInternalFlags = ["INCLUDEGITDATA"]
            }
          ]
    }
