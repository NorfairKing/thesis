{-# LANGUAGE TemplateHaskell #-}

module Thesis.DetailedProposal where

import Import hiding ((</>))

import Development.Shake

import Thesis.LaTeX

detailedProposalRules :: Rules ()
detailedProposalRules = simpleLaTeXRules detailedProposalSpec

detailedProposalSpec :: LaTeXRulesSpec
detailedProposalSpec =
    LaTeXRulesSpec
    { latexTopDir = $(mkRelDir "detailed-proposal")
    , latexPdfOutFile =
          $(mkRelFile "thesis-detailed-proposal-tom-sydney-kerckhove.pdf")
    , latexMainTexFileName = $(mkRelFile "proposal")
    }
