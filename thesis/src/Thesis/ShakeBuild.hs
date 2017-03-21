module Thesis.ShakeBuild where

import Development.Shake

import Thesis.Document
import Thesis.GitInfo

thesisShakeBuildRules :: Rules ()
thesisShakeBuildRules = do
    gitInfoRules
    documentRules
