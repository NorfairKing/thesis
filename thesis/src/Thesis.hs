module Thesis where

import Import

import Development.Shake

import Thesis.ShakeBuild

thesis :: IO ()
thesis =
    shakeArgs shakeOptions $ do
        thesisShakeBuildRules
        want [proposalOut]
