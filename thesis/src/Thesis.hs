module Thesis where

import Import

import Development.Shake

import Thesis.DetailedProposal
import Thesis.LaTeX
import Thesis.Proposal
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions $ do
        thesisShakeBuildRules
        want
            [ toFilePath $ pdfOutFile detailedProposalSpec
            , toFilePath $ pdfOutFile proposalSpec
            ]
