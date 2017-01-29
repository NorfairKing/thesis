module Thesis where

import Import

import Development.Shake
import Development.Shake.Path

import Thesis.DetailedProposal
import Thesis.LaTeX
import Thesis.Proposal
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions $ do
        thesisShakeBuildRules
        wantP [pdfOutFile detailedProposalSpec, pdfOutFile proposalSpec]
