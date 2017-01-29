{-# LANGUAGE TemplateHaskell #-}

module Thesis.Proposal where

import Import

import Development.Shake

import Thesis.LaTeX

proposalRules :: Rules ()
proposalRules = simpleLaTeXRules proposalSpec

proposalSpec :: LaTeXRulesSpec
proposalSpec =
    LaTeXRulesSpec
    { latexTopDir = $(mkRelDir "proposal")
    , latexPdfOutFile = $(mkRelFile "thesis-proposal-tom-sydney-kerckhove.pdf")
    , latexMainTexFileName = $(mkRelFile "proposal")
    }
