module Thesis.ShakeBuild where

import Development.Shake

import Thesis.DetailedProposal
import Thesis.Proposal

thesisShakeBuildRules :: Rules ()
thesisShakeBuildRules = do
    proposalRules
    detailedProposalRules
