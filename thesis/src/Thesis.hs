module Thesis where

import Import

import Development.Shake

import Thesis.Proposal
import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions $ do
        thesisShakeBuildRules
        want [proposalOut]
