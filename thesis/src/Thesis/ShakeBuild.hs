module Thesis.ShakeBuild where

import Development.Shake

import Thesis.Proposal

thesisShakeBuildRules :: Rules ()
thesisShakeBuildRules = proposalRules
