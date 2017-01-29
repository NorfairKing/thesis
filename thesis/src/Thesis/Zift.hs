module Thesis.Zift
    ( thesisZiftScript
    ) where

import Import

import Zifter

import Development.Shake

import Thesis.DetailedProposal
import Thesis.LaTeX
import Thesis.Proposal
import Thesis.ShakeBuild

thesisZiftScript :: ZiftScript ()
thesisZiftScript = do
    preprocessor $ pure ()
    checker $
        liftIO $
        shakeArgs shakeOptions $ do
            thesisShakeBuildRules
            want
                [ toFilePath $ pdfOutFile detailedProposalSpec
                , toFilePath $ pdfOutFile proposalSpec
                ]
